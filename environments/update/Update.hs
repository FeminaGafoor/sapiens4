{-# LANGUAGE CPP #-}
module Update (updateMode, updateAndRestart, candidateReady, preparingUpdate) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory hiding (executable)
import System.Environment
import System.FilePath
import System.IO
import UpdateState
import UpdatePlatform
import UpdateBackup
import Installation

candidateReady :: IO () -> IO ()
candidateReady activate = do
 preparing <- preparingUpdate
 when preparing $ do
  putStrLn "D2: ready" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  unless (command `elem` ["commit", ""]) (fail "Invalid Coordinator activation message")
  (root,version) <- installed
  state <- readUpdateState root
  unless (case state of Settled s -> current s == version; _ -> False) (fail "Candidate is not durably selected")
  activate
  unsetEnv "SAPIENS_UPDATE_ATTEMPT"
  (putStrLn "D2: active" >> hFlush stdout) `catch` \(_::IOException) -> pure ()

installed :: IO (FilePath,Version)
installed = installation >>= maybe (fail "Updates require an installed Sapiens version") pure

updateAndRestart :: (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestart command flushPreferences stop dataDirectory configDirectory = transaction `onException`
 (void (command "activate") `catch` \(_::IOException) -> pure ())
 where
 transaction = do
  let quiet = command "quiesce" >>= \result -> unless (result == "quiet") (fail "Sapiens is busy. Finish accepted work before updating.")
  quiet
  void (command "activate")
  next <- requestUpdate
  quiet
  flushPreferences
  (root,owner) <- installed
  withCandidate (versionExecutable root owner) ["--d2-update",versionName next,dataDirectory,configDirectory] [] $ \coordinator -> do
   (await coordinator "D2: coordinator" >> stop >> candidateOwnership coordinator True >> sendCandidate coordinator "handoff")
    `onException` candidateOwnership coordinator False

requestUpdate :: IO Version
requestUpdate = do
 (root,owner) <- installed
 nonce <- freshVersion
 let destination = versionPath root nonce
     payload = root </> (versionName nonce ++ "-" ++ payloadName)
 createDirectoryIfMissing True (root </> "versions")
#ifdef D2_FIXTURE
 fixture <- lookupEnv "SAPIENS_D2_FIXTURE_PAYLOAD"
 maybe (download payload) (\file -> B.readFile file >>= B.writeFile payload) fixture
#else
 download payload
 verifyPublisher (versionExecutable root owner) payload
#endif
 stagePayload payload destination
#ifndef D2_FIXTURE
 verifyPublisher (versionExecutable root owner) (versionExecutable root nonce)
#endif
 removeFile payload
 pure nonce
 where
 download path = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest "https://api.github.com/repos/PoulWalcker/sapiens4/releases/latest"
  response <- httpLbs request {requestHeaders=[("User-Agent","Sapiens4-Updater"),("Accept","application/vnd.github+json")]} manager
  value <- either fail pure (eitherDecode (responseBody response))
  assets <- either fail pure (parseEither (withObject "release" (\o -> o .: "assets")) value) :: IO [Value]
  let artifact = payloadName
      asset = withObject "release asset" $ \o -> (,) <$> o .: "name" <*> o .: "browser_download_url"
  urls <- forM assets (either fail pure . parseEither asset) :: IO [(String,String)]
  url <- maybe (fail "No update payload for this platform is available in the latest release") pure (lookup artifact urls)
  unless ("https://github.com/PoulWalcker/sapiens4/releases/download/" `isPrefixOf` url) (fail "Unexpected update download origin")
  transfer <- parseRequest url
  withResponse transfer manager $ \result -> withBinaryFile path WriteMode $ \file ->
   let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
   in loop


updateMode :: [String] -> IO Bool
updateMode args = case args of
 ["--d2-update",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseEither parseJSON (toJSON name))
  putStrLn "D2: coordinator" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  when (command == "handoff") $
   coordinate (Just (version,dataDirectory,configDirectory)) `catch` \(problem::SomeException) -> do
    showUpdateError (displayException problem)
    coordinate Nothing
  pure True
 ["--d2-recover"] -> coordinate Nothing >> pure True
 _ -> pure False

data Dispatch = Running | Launch Version | Recover
coordinate :: Maybe (Version,FilePath,FilePath) -> IO ()
coordinate requested = do
 (root,owner) <- installed
 dispatch <- withLease (leasePath root) True $ \lease -> do
  state <- readUpdateState root
  case (state,requested) of
   (Pending pending,_) -> do
    unless (owner == current (original pending)) (fail "Recovery must run through retained original version")
    restoreSnapshot root pending
    writeUpdateState root (Settled (original pending))
    pure (Launch (current (original pending)))
   (Settled selected,Nothing) -> pure (Launch (current selected))
   (Settled selected,Just (next,dataDirectory,configDirectory)) -> do
    unless (owner == current selected && next /= owner && Just next /= (previousVersion <$> previous selected)) (fail "Update selection changed")
    originalData <- canonicalizePath dataDirectory
    originalConfig <- canonicalizePath configDirectory
    nonce <- freshVersion
    createDirectoryIfMissing True (root </> "backups")
    let pending = PendingUpdate selected next nonce originalData originalConfig ""
    fingerprint <- withRootLocks originalData $ snapshot (backupPath root pending) originalData originalConfig
    let saved = pending {backupDigest=fingerprint}
    writeUpdateState root (Pending saved)
    downgradeLease lease
    result <- try (runCandidate root saved)
    case result of
     Right () -> do
      forM_ (previous selected) $ \old ->
       mapM_ removePathForcibly [versionPath root (previousVersion old),root </> "backups" </> versionName (previousBackup old)]
        `catch` \(_::IOException) -> pure ()
      pure Running
     Left (_::SomeException) -> pure Recover
 case dispatch of
  Running -> pure ()
  Launch version -> launchVersion root version []
  Recover -> coordinate Nothing

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = withCandidate (versionExecutable root (target pending)) []
 [("SAPIENS_UPDATE_ATTEMPT",versionName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)] $ \candidate -> do
  let protocol = do
       await candidate "D2: ready"
       state <- readUpdateState root
       unless (state == Pending pending) (fail "Update decision changed before Ready")
       writeUpdateState root (Settled (Selection (target pending) (Just (Previous (current (original pending)) (attempt pending) (backupDigest pending)))))
       candidateOwnership candidate True
       sendCandidate candidate "commit"
       await candidate "D2: active"
  protocol `catch` \(problem::SomeException) -> do
   state <- readUpdateState root
   case state of
    Settled selected | current selected == target pending -> do
     candidateOwnership candidate True
     alive <- candidateAlive candidate
     unless alive (throwIO problem)
    _ -> candidateOwnership candidate False >> throwIO problem

await :: Candidate -> String -> IO ()
await candidate expected = do
 message <- candidateMessage candidate
 when (message == "D2: failed") (fail "Candidate could not prepare the workspace")
 unless (message == expected) (await candidate expected)
