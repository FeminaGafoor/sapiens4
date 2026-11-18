module Installation (withInstalledVersion, installation, writeUpdateState, freshVersion, leasePath, preparingUpdate, starter, outsideInstallation) where

import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, toJSON, parseJSON)
import Data.Aeson.Types (parseEither)
import Data.Maybe (maybeToList)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Environment
import System.Directory (canonicalizePath, doesFileExist, removeFile, removePathForcibly)
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
writeUpdateState root state = do
 nonce <- freshVersion
 publish (statePath root) (statePath root ++ "." ++ versionName nonce ++ ".tmp") (B.toStrict (encode state))
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
 let lock = leasePath root
 let withDecision = if args == ["--uninstall"] then withIdleLease lock else withLease lock (not (null args)) . const
 withDecision $ case args of
  ["--initialize",name] -> do
   version <- either fail pure (parseEither parseJSON (toJSON name))
   exists <- doesFileExist (statePath root)
   when exists (fail "An installation decision already exists")
   validatePath (versionExecutable root version)
   writeUpdateState root (Settled (Selection version Nothing))
  ["--uninstall"] -> do
   validatePath (statePath root)
   selected <- readUpdateState root >>= \state -> case state of
    Settled selection -> pure selection
    Pending _ -> fail "Recover the interrupted Sapiens update before uninstalling."
   let versions = current selected : map previousVersion (maybeToList (previous selected))
       owned = map (versionPath root) versions ++
        [root </> "backups" </> versionName (previousBackup old) | old <- maybeToList (previous selected)]
   forM_ owned $ \path -> pathPresent path >>= \present -> when present (removePathForcibly path)
   removeFile (statePath root)
  [] -> do
   state <- readUpdateState root
   case state of
    Settled selected -> launchVersion root (current selected) []
    Pending pending -> launchVersion root (current (original pending)) ["--d2-recover"]
  _ -> fail "Unsupported Starter command"
