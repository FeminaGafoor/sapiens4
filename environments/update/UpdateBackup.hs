module UpdateBackup (snapshot, restoreSnapshot, withRootLocks, validateRoots) where

import Control.Exception
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List (nub, sort)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.FileLock
import UpdatePlatform
import UpdateState

data EntryKind = FileEntry | DirectoryEntry deriving Eq
data TreeEntry = TreeEntry { relativePath :: FilePath, entryKind :: EntryKind } deriving Eq
data Entry = Entry { location :: TreeEntry, fingerprint :: Sha256 } deriving Eq

data Inventory = Inventory
 { inventoryRoot :: FilePath, writerLocks :: [FilePath], exclusions :: [FilePath], entries :: [Entry] }
 deriving Eq

data BackupInventory = BackupInventory { dataInventory :: Inventory, configInventory :: Inventory }
 deriving Eq

-- The digest covers the existing ordered array format, including Boolean entry kinds.
instance ToJSON Entry where
 toJSON entry = toJSON (relativePath (location entry),entryKind (location entry) == DirectoryEntry,fingerprint entry)

instance ToJSON Inventory where
 toJSON (Inventory root locks excluded saved) = toJSON (root,locks,excluded,saved)

instance ToJSON BackupInventory where
 toJSON (BackupInventory savedData savedConfig) = toJSON [savedData,savedConfig]

backupRoots :: FilePath -> UpdateRoots
backupRoots destination = UpdateRoots {dataRoot=destination </> "0", configRoot=destination </> "1"}

managedChildren :: FilePath -> IO [FilePath]
managedChildren root = do
 validatePath root
 let path = root </> "host-corpora.json"
 exists <- pathPresent path
 if not exists then pure [] else do
  names <- B.readFile path >>= either fail pure . (eitherDecodeStrict >=> parseEither registry)
  forM names $ \name -> case canonicalUUID name of
   Just _ -> pure (root </> "corporas" </> T.unpack name)
   _ -> fail "Invalid registered child Corpora"
 where registry = withObject "Corpora registry" (\o -> o .:? "locals" .!= [])

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

tree :: FilePath -> [FilePath] -> [FilePath] -> IO [TreeEntry]
tree root locks excluded = walk ""
 where
 walk relative | any (`containsPath` relative) excluded || any (equalFilePath relative) locks = pure []
 walk relative = do
  let path = root </> relative
  validatePath path
  directory <- doesDirectoryExist path
  children <- if not directory then pure [] else sort <$> listDirectory path >>= fmap concat . mapM (walk . (relative </>))
  pure (TreeEntry relative (if directory then DirectoryEntry else FileEntry):children)

inventory :: FilePath -> [FilePath] -> [FilePath] -> IO [Entry]
inventory root locks excluded = tree root locks excluded >>= mapM (\entry ->
 Entry entry . sha256 . L.fromStrict <$> metadata (root </> relativePath entry))

snapshot :: FilePath -> UpdateRoots -> IO Sha256
snapshot destination live = do
 unless (null profileExclusions) $ do
  corpora <- managedRoots (dataRoot live)
  forM_ (configRoot live:corpora) $ \root ->
   forM_ (nub (map takeDirectory profileExclusions)) $ \parent -> do
    let path = root </> parent
    exists <- pathPresent path
    unless exists (createDirectory path)
    validatePath path
    directory <- doesDirectoryExist path
    unless directory (fail ("Expected a browser profile directory: " ++ path))
 original <- inventories live live
 let backup = backupRoots destination
 createDirectory destination
 copyTree (dataRoot live) (dataRoot backup) (entries (dataInventory original))
 copyTree (configRoot live) (configRoot backup) (entries (configInventory original))
 copied <- inventories live backup
 unless (copied == original) (fail "Backup verification failed; original data are unchanged")
 pure (sha256 (encode original))

validateRoots :: FilePath -> UpdateRoots -> IO ()
validateRoots installation (UpdateRoots dataPath configPath) = do
 mapM_ validatePath [installation,dataPath,configPath]
 let overlaps a b = containsPath a b || containsPath b a
 when (overlaps dataPath configPath || any (overlaps installation) [dataPath,configPath]) $
  fail "Installation, Data and Config must be separate roots for updates"

inventories :: UpdateRoots -> UpdateRoots -> IO BackupInventory
inventories live source = do
 corpora <- managedRoots (dataRoot source)
 let dataPath = dataRoot source
     locks = map (makeRelative dataPath . (</> "host.lock")) corpora
     relative child = if child == dataPath then "" else makeRelative dataPath child
     profiles = [relative child </> profile | child <- corpora, profile <- reservationName:profileExclusions]
     configExcluded = configOwnerName:configWriterLockName:profileExclusions
 savedData <- Inventory (dataRoot live) locks profiles <$> inventory dataPath locks profiles
 savedConfig <- Inventory (configRoot live) [] configExcluded <$> inventory (configRoot source) [] configExcluded
 pure (BackupInventory savedData savedConfig)

copyTree :: FilePath -> FilePath -> [Entry] -> IO ()
copyTree source destination saved = do
 forM_ (map location saved) $ \(TreeEntry relative kind) -> case kind of
  DirectoryEntry -> createDirectoryIfMissing True (destination </> relative)
  FileEntry -> copyOwned (source </> relative) (destination </> relative)
 forM_ (map location saved) $ \(TreeEntry relative kind) ->
  copyMetadata (kind == DirectoryEntry) (source </> relative) (destination </> relative)

restoreSnapshot :: FilePath -> PendingUpdate -> IO () -> IO ()
restoreSnapshot installation pending settle = do
 let backup = backupRoots (backupPath installation (attempt pending))
     live = roots pending
     originalData = dataRoot live
     originalConfig = configRoot live
 validateRoots installation live
 withRootIdentity originalData $ \dataId -> withRootIdentity originalConfig $ \configId -> do
  unless (dataId == dataIdentity pending && configId == configIdentity pending) $
   fail "The original Data or Config directory was replaced. Recovery refused; backup retained."
  configOwner <- readBinding (originalConfig </> configOwnerName)
  unless (configOwner == Just installation) $
   fail "Config ownership changed. Recovery refused; backup retained."
  original <- inventories live backup
  unless (sha256 (encode original) == backupDigest pending) (fail "Backup changed or is incomplete; recovery refused")
  let childLocks = map (originalData </>) (nub (drop 1 (writerLocks (dataInventory original))))
  withLock (originalData </> "host.lock") $ do
   reservation <- readBinding (originalData </> reservationName)
   unless (reservation == Just (DataReservation installation (attempt pending))) $
    fail "Data Root update ownership changed. Recovery refused; backup retained."
   foldr withSurvivingLock (restore backup original >> settle) childLocks
 where
 restore backup original = do
  let savedData = dataInventory original
      savedConfig = configInventory original
      observe saved = tree (inventoryRoot saved) (writerLocks saved) (exclusions saved)
  liveData <- observe savedData
  liveConfig <- observe savedConfig
  restoreTree (dataRoot backup) savedData liveData
  restoreTree (configRoot backup) savedConfig liveConfig

restoreTree :: FilePath -> Inventory -> [TreeEntry] -> IO ()
restoreTree backup saved existing = do
 let root = inventoryRoot saved
 forM_ (removals saved existing) $ \(TreeEntry relative kind) -> do
  let path = root </> relative
  case kind of
   FileEntry -> do
    permissions <- getPermissions path
    setPermissions path permissions {writable=True}
    removeFile path
   DirectoryEntry -> removeDirectory path
 copyTree backup root (entries saved)
 actual <- inventory root (writerLocks saved) (exclusions saved)
 unless (actual == entries saved) (fail "Recovery verification failed; original backup retained for retry")

removals :: Inventory -> [TreeEntry] -> [TreeEntry]
removals saved = filter shouldRemove . reverse
 where
 savedDirectories = [path | Entry (TreeEntry path DirectoryEntry) _ <- entries saved]
 protected = writerLocks saved ++ exclusions saved
 shouldRemove (TreeEntry path kind) = not (null path) && case kind of
  FileEntry -> True
  DirectoryEntry -> not (any (equalFilePath path) savedDirectories || any (containsPath path) protected)
