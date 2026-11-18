module UpdateBackup (snapshot, restoreSnapshot, withRootLocks, validateRoots) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Crypto.Hash.SHA256 as SHA
import Data.List (nub, sort)
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

managedChildren :: FilePath -> IO [FilePath]
managedChildren root = do
 validatePath root
 let path = root </> "host-corpora.json"
 exists <- pathPresent path
 if not exists then pure [] else do
  value <- B.readFile path >>= either fail pure . eitherDecodeStrict
  names <- either fail pure (parseEither (withObject "Corpora registry" (\o -> o .:? "locals" .!= [])) value)
  forM names $ \name -> case UUID.fromText name of
   Just ident | UUID.toText ident == name -> pure (root </> "corporas" </> T.unpack name)
   _ -> fail "Invalid registered child Corpora"

managedRoots :: FilePath -> IO [FilePath]
managedRoots root = managedChildren root >>= fmap ((root:) . concat) . mapM managedRoots

withRootLocks :: FilePath -> IO a -> IO a
withRootLocks root action = withLock (root </> "host.lock") $ do
 assertDataAvailable root
 children <- managedChildren root
 foldr withRootLocks action (sort (nub children))

withLock :: FilePath -> IO a -> IO a
withLock path action = do
 validatePath (takeDirectory path)
 void (pathPresent path)
 bracket (tryLockFile path Exclusive >>= maybe (fail "A Corpora writer is still running; update refused") pure)
  unlockFile (const action)

withSurvivingLock :: FilePath -> IO a -> IO a
withSurvivingLock path action = do
 let parent = takeDirectory path
 exists <- pathPresent parent
 directory <- if exists then doesDirectoryExist parent else pure False
 if directory then withLock path action else action

tree :: FilePath -> [FilePath] -> [FilePath] -> IO [(FilePath,Bool)]
tree root locks excluded = walk ""
 where
 walk relative | any (`containsPath` relative) excluded || any (equalFilePath relative) locks = pure []
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
 original <- inventories dataPath configPath dataPath configPath
 createDirectory destination
 forM_ (zip [0::Int ..] original) $ \(index,Inventory root _ _ entries) ->
  copyTree root (destination </> show index) entries
 copied <- inventories dataPath configPath (destination </> "0") (destination </> "1")
 unless (copied == original) (fail "Backup verification failed; original data are unchanged")
 pure (hash (L.toStrict (encode original)))

validateRoots :: FilePath -> FilePath -> FilePath -> IO ()
validateRoots installation dataPath configPath = do
 mapM_ validatePath [installation,dataPath,configPath]
 let overlaps a b = containsPath a b || containsPath b a
 when (overlaps dataPath configPath || any (overlaps installation) [dataPath,configPath]) $
  fail "Installation, Data and Config must be separate roots for updates"

inventories :: FilePath -> FilePath -> FilePath -> FilePath -> IO [Inventory]
inventories dataPath configPath sourceData sourceConfig = do
 corpora <- managedRoots sourceData
 let locks = map (makeRelative sourceData . (</> "host.lock")) corpora
     relative child = if child == sourceData then "" else makeRelative sourceData child
     profiles = [relative child </> profile | child <- corpora, profile <- reservationName:profileExclusions]
 dataEntries <- inventory sourceData locks profiles
 let configExcluded = configOwnerName:profileExclusions
 configEntries <- inventory sourceConfig [] configExcluded
 pure [Inventory dataPath locks profiles dataEntries,Inventory configPath [] configExcluded configEntries]

copyTree :: FilePath -> FilePath -> [Entry] -> IO ()
copyTree source destination entries = do
 forM_ entries $ \(relative,directory,_) -> if directory
  then createDirectoryIfMissing True (destination </> relative)
  else copyOwned (source </> relative) (destination </> relative)
 forM_ entries $ \(relative,directory,_) ->
  copyMetadata directory (source </> relative) (destination </> relative)

restoreSnapshot :: FilePath -> PendingUpdate -> IO () -> IO ()
restoreSnapshot installation pending settle = do
 let backup = backupPath installation (attempt pending)
     originalData = dataRoot pending
     originalConfig = configRoot pending
 validateRoots installation originalData originalConfig
 withRootIdentity originalData $ \dataId -> withRootIdentity originalConfig $ \configId -> do
  unless (dataId == dataIdentity pending && configId == configIdentity pending) $
   fail "The original Data or Config directory was replaced. Recovery refused; backup retained."
  configOwner <- readBinding (originalConfig </> configOwnerName)
  unless (configOwner == Just installation) $
   fail "Config ownership changed. Recovery refused; backup retained."
  original <- inventories originalData originalConfig (backup </> "0") (backup </> "1")
  unless (hash (L.toStrict (encode original)) == backupDigest pending) (fail "Backup changed or is incomplete; recovery refused")
  let childLocks = nub [root </> lock | Inventory root locks _ _ <- original, lock <- drop 1 locks]
  withLock (originalData </> "host.lock") $ do
   reservation <- readBinding (originalData </> reservationName)
   unless (reservation == Just (installation,attempt pending)) $
    fail "Data Root update ownership changed. Recovery refused; backup retained."
   foldr withSurvivingLock (restore backup original >> settle) childLocks
 where
 restore backup original = do
  live <- forM original $ \(Inventory root locks excluded _) -> tree root locks excluded
  forM_ (zip3 [0::Int ..] original live) $ \(index,Inventory root locks excluded entries,existing) -> do
   forM_ (reverse existing) $ \(relative,directory) -> unless (null relative) $ do
    let path = root </> relative
    if not directory then do
     permissions <- getPermissions path
     setPermissions path permissions {writable=True}
     removeFile path
    else when (not (any (equalFilePath relative) [p | (p,True,_) <- entries])
      && not (any (containsPath relative) (locks++excluded))) (removeDirectory path)
   copyTree (backup </> show index) root entries
   actual <- inventory root locks excluded
   unless (actual == entries) (fail "Recovery verification failed; original backup retained for retry")
