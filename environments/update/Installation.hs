module Installation (withInstalledVersion, installation, writeUpdateState, reserveDataRoot, claimConfigRoot, checkCandidateData, freshVersion, leasePath, preparingUpdate, starter, outsideInstallation) where

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

outsideInstallation :: FilePath -> IO ()
outsideInstallation directory = do
 home <- installation
 forM_ home $ \(root,_) -> do
  path <- canonicalizePath directory
  when (containsPath root path || containsPath path root) $
   fail "Choose a Data or Config folder outside the Sapiens installation."

writeUpdateState :: FilePath -> UpdateState -> IO ()
writeUpdateState root = publishJSON publish (statePath root)

reserveDataRoot :: FilePath -> FilePath -> Version -> IO ()
reserveDataRoot directory root version = publishJSON publish (directory </> reservationName) (root,version)

-- Call while holding this installation's lease. To transfer ownership, also
-- obtain the other installation's existing lease immediately or refuse.
claimConfigRoot :: FilePath -> IO ()
claimConfigRoot directory = do
 home <- installation
 forM_ home $ \(root,_) -> do
  createDirectoryIfMissing True directory
  validatePath directory
  let binding = directory </> configOwnerName
  void (pathPresent binding)
  owner <- readBinding binding
  case owner of
   Nothing -> publishJSON publishNew binding root
   Just other | equalFilePath other root -> pure ()
   Just other -> do
    unless (isAbsolute other) (fail "Invalid Config ownership")
    validatePath (leasePath other)
    withIdleLease (leasePath other) $ do
     decision <- readUpdateState other
     when (case decision of Pending _ -> True; Settled _ -> False) $
      fail "This Config folder is needed to recover another Sapiens installation. Use its installed shortcut."
     currentOwner <- readBinding binding
     unless (currentOwner == owner) (fail "Config ownership changed during startup. Relaunch Sapiens.")
     publishJSON publish binding root

checkCandidateData :: FilePath -> IO ()
checkCandidateData directory = do
 preparing <- preparingUpdate
 when preparing $ do
  (root,_) <- installation >>= maybe (fail "Candidate requires an installation") pure
  path <- canonicalizePath directory
  state <- readUpdateState root
  unless (case state of Pending pending -> equalFilePath path (dataRoot pending); Settled _ -> False) $
   fail "Candidate Data Root differs from the reserved directory."

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
