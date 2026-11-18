module UpdateBackup (snapshot, restoreSnapshot, withRootLocks) where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.List (sort)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import Numeric (showHex)
import System.Directory
import System.FilePath
import System.FileLock
import UpdatePlatform
import UpdateState

type Entry = (FilePath, Bool, String)
data Inventory = Inventory FilePath [FilePath] [FilePath] [Entry] deriving (Eq,Show)
instance ToJSON Inventory where
 toJSON (Inventory root locks excluded entries) = toJSON (root,locks,excluded,entries)
hash :: B.ByteString -> String
hash = concatMap (\b -> let s=showHex b "" in replicate (2-length s) '0' ++ s) . B.unpack . SHA.hash

managedRoots :: FilePath -> IO [FilePath]
managedRoots root = do
 validatePath root
 let path = root </> "host-corpora.json"
 exists <- doesFileExist path
 children <- if not exists then pure [] else do
  validatePath path
  value <- B.readFile path >>= either fail pure . eitherDecodeStrict
  names <- either fail pure (parseEitherLocals value)
  forM names $ \name -> case UUID.fromText name of
   Just ident | UUID.toText ident == name -> pure (root </> "corporas" </> T.unpack name)
   _ -> fail "Invalid registered child Corpora"
 (root:) . concat <$> mapM managedRoots children
 where
 parseEitherLocals value = case fromJSON value :: Result (DataMap) of
  Error problem -> Left problem
  Success (DataMap names) -> Right names
newtype DataMap = DataMap [T.Text]
instance FromJSON DataMap where parseJSON = withObject "Corpora registry" (\o -> DataMap <$> o .:? "locals" .!= [])

withRootLocks :: FilePath -> IO a -> IO a
withRootLocks root action = do
 roots <- managedRoots root
 withLocks (sort [p </> "host.lock" | p <- roots]) action

withLocks :: [FilePath] -> IO a -> IO a
withLocks paths action = acquire paths
 where
 acquire [] = action
 acquire (p:ps) = do
  validatePath (takeDirectory p)
  exists <- doesPathExist p
  when exists (validatePath p)
  bracket (tryLockFile p Exclusive >>= maybe (fail "A Corpora writer is still running; update refused") pure) unlockFile (const (acquire ps))

tree :: FilePath -> [FilePath] -> [FilePath] -> IO [(FilePath,Bool)]
tree root locks excluded = walk ""
 where
 walk relative | any (`containsPath` relative) excluded || relative `elem` locks = pure []
 walk relative = do
  let path = root </> relative
  validatePath path
  directory <- doesDirectoryExist path
  children <- if not directory then pure [] else sort <$> listDirectory path >>= fmap concat . mapM (walk . (relative </>))
  pure ((relative,directory):children)

inventory :: FilePath -> [FilePath] -> [FilePath] -> IO [Entry]
inventory root locks excluded = tree root locks excluded >>= mapM (\(path,directory) -> do
 fingerprint <- hash <$> metadata (root </> path)
 pure (path,directory,fingerprint))

snapshot :: FilePath -> FilePath -> FilePath -> IO String
snapshot destination dataDirectory configDirectory = do
 dataPath <- canonicalizePath dataDirectory
 configPath <- canonicalizePath configDirectory
 when (containsPath dataPath configPath || containsPath configPath dataPath) $
  fail "Data and Config must be separate roots for updates"
 original <- inventories dataPath configPath dataPath configPath
 createDirectory destination
 forM_ (zip [0::Int ..] original) $ \(index,Inventory root _ _ entries) ->
  copyTree root (destination </> show index) entries
 copied <- inventories dataPath configPath (destination </> "0") (destination </> "1")
 unless (copied == original) (fail "Backup verification failed; original data are unchanged")
 pure (hash (L.toStrict (encode original)))

inventories :: FilePath -> FilePath -> FilePath -> FilePath -> IO [Inventory]
inventories dataPath configPath sourceData sourceConfig = do
 corpora <- managedRoots sourceData
 let locks = map (makeRelative sourceData . (</> "host.lock")) corpora
     relative child = if child == sourceData then "" else makeRelative sourceData child
     profiles = [relative child </> profile | child <- corpora, profile <- profileExclusions]
 dataEntries <- inventory sourceData locks profiles
 configEntries <- inventory sourceConfig [] profileExclusions
 pure [Inventory dataPath locks profiles dataEntries,Inventory configPath [] profileExclusions configEntries]

copyTree :: FilePath -> FilePath -> [Entry] -> IO ()
copyTree source destination entries = do
 forM_ entries $ \(relative,directory,_) -> if directory
  then createDirectoryIfMissing True (destination </> relative)
  else copyOwned (source </> relative) (destination </> relative)
 forM_ entries $ \(relative,_,_) ->
  copyMetadata (source </> relative) (destination </> relative)

restoreSnapshot :: FilePath -> PendingUpdate -> IO ()
restoreSnapshot installation pending = do
 let backup = backupPath installation pending
 original <- inventories (dataRoot pending) (configRoot pending) (backup </> "0") (backup </> "1")
 unless (hash (L.toStrict (encode original)) == backupDigest pending) (fail "Backup changed or is incomplete; recovery refused")
 withLocks [root </> lock | Inventory root locks _ _ <- original, lock <- locks] $ forM_ (zip [0::Int ..] original) $ \(index,Inventory root locks excluded entries) -> do
  live <- tree root locks excluded
  forM_ (reverse live) $ \(relative,directory) -> unless (null relative) $ do
   let path = root </> relative
   if not directory then do
    permissions <- getPermissions path
    setPermissions path permissions {writable=True}
    removeFile path
   else when (relative `notElem` [p | (p,True,_) <- entries] && not (any (containsPath relative) (locks++excluded))) (removeDirectory path)
  copyTree (backup </> show index) root entries
  actual <- inventory root locks excluded
  unless (actual == entries) (fail "Recovery verification failed; original backup retained for retry")
