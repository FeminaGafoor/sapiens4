module Installation (withInstalledVersion, installation, writeUpdateState, reserveDataRoot, withConfigAccess, claimConfigRoot, checkCandidateData, freshVersion, leasePath, preparingUpdate, starter) where

import Control.Exception (IOException, catch)
import Control.Monad
import qualified Data.ByteString as Strict
import qualified Data.ByteString.Lazy as B
import Data.Aeson (ToJSON, encode, toJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Environment
import System.Directory (canonicalizePath, createDirectoryIfMissing, doesFileExist)
import System.FilePath
import UpdateState
import UpdatePlatform

freshVersion :: IO Version
freshVersion = Version . UUID.toString <$> UUID.nextRandom
leasePath :: FilePath -> FilePath
leasePath root = root </> "installation.lock"
installation :: IO (Maybe (FilePath,Version))
installation = getExecutablePath >>= installationOf

writeUpdateState :: FilePath -> UpdateState -> IO ()
writeUpdateState root = publishJSON publish (statePath root)

reserveDataRoot :: FilePath -> FilePath -> Version -> IO ()
reserveDataRoot directory root version = publishJSON publish (directory </> reservationName) (root,version)

withConfigAccess :: FilePath -> IO a -> IO a
withConfigAccess directory action = installation >>= \home -> case home of
 Just (root,_) -> claim root directory >> action
 Nothing -> do
  createDirectoryIfMissing True directory
  withLease (directory </> configWriterLockName) False $ \_ -> do
   owner <- configOwner directory
   unless (null owner) (fail "This Config folder belongs to an installed Sapiens. Choose a separate Config folder.")
   action

claimConfigRoot :: FilePath -> IO ()
claimConfigRoot directory = installation >>= mapM_ (\(root,_) -> claim root directory)

configOwner :: FilePath -> IO (Maybe FilePath)
configOwner directory = readBinding (directory </> configOwnerName)

claim :: FilePath -> FilePath -> IO ()
claim root directory = do
 createDirectoryIfMissing True directory
 validatePath directory
 let binding = directory </> configOwnerName
     owns = maybe False (equalFilePath root)
 void (pathPresent binding)
 owner <- configOwner directory
 unless (owns owner) $
  withIdleLease (directory </> configWriterLockName) (do
   void (pathPresent binding)
   currentOwner <- configOwner directory
   case currentOwner of
    Nothing -> publishJSON publishNew binding root
    Just other | equalFilePath other root -> pure ()
    Just other -> do
     unless (isAbsolute other) (fail "Invalid Config ownership")
     validatePath (statePath other)
     withIdleLease (leasePath other) $ do
      decision <- readUpdateState other
      when (case decision of Pending _ -> True; Settled _ -> False) $
       fail "This Config folder is needed to recover another Sapiens installation. Use its installed shortcut."
      publishJSON publish binding root)
  `catch` \(problem :: IOException) ->
   fail ("Cannot claim this Config folder. Close other Sapiens writers or choose a separate Config folder. " ++ show problem)

checkCandidateData :: FilePath -> IO Bool
checkCandidateData directory = do
 preparing <- preparingUpdate
 when preparing $ do
  (root,_) <- installation >>= maybe (fail "Candidate requires an installation") pure
  path <- canonicalizePath directory
  state <- readUpdateState root
  unless (case state of Pending pending -> equalFilePath path (dataRoot pending); Settled _ -> False) $
   fail "Candidate Data Root differs from the reserved directory."
 pure preparing

publishJSON :: ToJSON a => (FilePath -> FilePath -> Strict.ByteString -> IO ()) -> FilePath -> a -> IO ()
publishJSON publishFile path value = do
 nonce <- freshVersion
 publishFile path (path ++ "." ++ versionName nonce ++ ".tmp") (B.toStrict (encode value))
withInstalledVersion :: IO a -> IO a
withInstalledVersion action = installation >>= maybe action (\(root,version) -> withLease (leasePath root) False $ \_ -> do
 state <- readUpdateState root
 allowed <- case state of
  Settled selection -> pure (version == current selection)
  Pending pending -> do
   nonce <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
   pure (version == target pending && nonce == Just (versionName (attempt pending)))
 unless allowed (fail "This version is not authorized to open application data. Launch Sapiens from its installed shortcut.")
 action)

preparingUpdate :: IO Bool
preparingUpdate = maybe False (const True) <$> lookupEnv "SAPIENS_UPDATE_ATTEMPT"

starter :: [String] -> IO ()
starter args = do
 root <- starterDirectory
 withLease (leasePath root) (not (null args)) $ \_ -> case args of
  ["--initialize",name] -> do
   version <- either fail pure (parseEither parseJSON (toJSON name))
   exists <- doesFileExist (statePath root)
   when exists (fail "An installation decision already exists")
   validatePath (versionExecutable root version)
   writeUpdateState root (Settled (Selection version Nothing))
  [] -> do
   state <- readUpdateState root
   case state of
    Settled selected -> launchVersion root (current selected) []
    Pending pending -> launchVersion root (current (original pending)) ["--update-recover"]
  _ -> fail "Unsupported Starter command"
