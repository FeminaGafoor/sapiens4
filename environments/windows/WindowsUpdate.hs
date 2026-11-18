{-# LANGUAGE CPP #-}
module WindowsUpdate (updateMode, requestUpdate, candidateReady, preparingUpdate) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.List (isPrefixOf)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory hiding (executable)
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Info (arch)
import System.Process
import System.Process.Internals (ProcessHandle__(..),withProcessHandle)
import UpdateState
import UpdateNative
import UpdateBackup
import Installation
import Native (waitProcessHandle)

preparingUpdate :: IO Bool
preparingUpdate = (==Just "1") <$> lookupEnv "SAPIENS_UPDATE_PREPARING"

candidateReady :: IO () -> IO ()
candidateReady activate = do
 preparing <- preparingUpdate
 when preparing $ do
  hSetBuffering stdout LineBuffering
  putStrLn "D2: ready"
  command <- getLine `catch` \(_::IOException) -> pure ""
  unless (command `elem` ["commit", ""]) (fail "Invalid Coordinator activation message")
  location <- installation >>= maybe (fail "Candidate has no installation") pure
  state <- readUpdateState (fst location)
  unless (case state of Settled s -> current s == snd location; _ -> False) (fail "Candidate is not durably selected")
  unsetEnv "SAPIENS_UPDATE_PREPARING"
  unsetEnv "SAPIENS_UPDATE_ATTEMPT"
  activate
  putStrLn "D2: active" `catch` \(_::IOException) -> pure ()

-- No download begins until the user invokes Update & Restart.
requestUpdate :: IO Version
requestUpdate = do
 (root,_) <- installation >>= maybe (fail "Updates require an installed Sapiens version") pure
 currentExe <- getExecutablePath
 nonce <- freshVersion
 let destination = versionPath root nonce
     payload = root </> ("payload-" ++ versionName nonce ++ ".exe")
 createDirectoryIfMissing True (root </> "versions")
#ifdef D2_FIXTURE
 fixture <- lookupEnv "SAPIENS_D2_FIXTURE_PAYLOAD"
 case fixture of
  Just file -> B.readFile file >>= B.writeFile payload
  Nothing -> download payload
#else
 download payload
#endif
#ifndef D2_FIXTURE
 verifyPublisher currentExe payload
#endif
 createDirectory destination
 code <- waitForProcess =<< (\(_,_,_,child) -> pure child) =<< createProcess (proc payload
  ["/VERYSILENT","/SUPPRESSMSGBOXES","/NORESTART","/DIR="++destination]) {close_fds=True}
 unless (code == ExitSuccess) (fail "Update payload could not be staged; current version is unchanged")
 let nextExe = destination </> "Sapiens4.exe"
 exists <- doesFileExist nextExe
 unless exists (fail "Update payload is incomplete")
#ifndef D2_FIXTURE
 verifyPublisher currentExe nextExe
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
  let artifact = "Sapiens-Payload-" ++ (if arch=="aarch64" then "win-arm64" else "win-x64") ++ ".exe"
      asset = withObject "release asset" $ \o -> (,) <$> o .: "name" <*> o .: "browser_download_url"
  urls <- forM assets (either fail pure . parseEither asset) :: IO [(String,String)]
  url <- maybe (fail "No Windows update payload is available in the latest release") pure (lookup artifact urls)
  unless ("https://github.com/PoulWalcker/sapiens4/releases/download/" `isPrefixOf` url) (fail "Unexpected update download origin")
  transfer <- parseRequest url
  withResponse transfer manager $ \result -> withBinaryFile path WriteMode $ \file ->
   let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
   in loop

updateMode :: [String] -> IO Bool
updateMode args = case args of
 ["--d2-update",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseEither parseJSON (toJSON name))
  runCoordinator (Just (version,dataDirectory,configDirectory))
  pure True
 ["--d2-recover"] -> runCoordinator Nothing >> pure True
 _ -> pure False

runCoordinator :: Maybe (Version,FilePath,FilePath) -> IO ()
runCoordinator requested = coordinate requested `catch` \(errorValue::SomeException) -> do
 showUpdateError (displayException errorValue)
 location <- installation
 forM_ location $ \(root,_) -> do
  state <- readUpdateState root
  case state of
   Settled selected -> launch root (current selected)
   Pending _ -> pure () -- Failed recovery must keep storage closed for retry.

coordinate :: Maybe (Version,FilePath,FilePath) -> IO ()
coordinate requested = do
 (root,owner) <- installation >>= maybe (fail "Update Coordinator must run from retained application version") pure
 decision <- withLease (leasePath root) True $ \lease -> do
  state <- readUpdateState root
  case state of
   Pending pending -> do
    unless (owner == current (original pending)) (fail "Recovery must run through retained original version")
    restoreSnapshot root pending
    writeUpdateState root (Settled (original pending))
    pure (Just (current (original pending)))
   Settled selected -> case requested of
    Nothing -> pure (Just (current selected))
    Just (next,dataDirectory,configDirectory) -> do
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
       forM_ (previous selected) $ \old -> do
        let discard = mapM_ removePathForcibly [versionPath root (previousVersion old),root </> "backups" </> versionName (previousBackup old)]
        discard `catch` \(_::IOException) -> pure ()
       pure Nothing
      Left (e::SomeException) -> do
       -- Leave Pending untouched. Exclusive reacquisition below proves all
       -- installation writers have left before any restoration is attempted.
       hPutStrLn stderr (displayException e)
       pure (Just owner)
 case decision of
  Nothing -> pure ()
  Just version -> do
   settled <- withLease (leasePath root) True $ \_ -> do
    state <- readUpdateState root
    case state of
     Pending pending -> do
      unless (version == current (original pending)) (fail "Recovery owner changed")
      restoreSnapshot root pending
      writeUpdateState root (Settled (original pending))
      pure (current (original pending))
     Settled selected -> pure (current selected)
   launch root settled

launch :: FilePath -> Version -> IO ()
launch root version = do
 env <- getEnvironment
 void $ createProcess (proc (versionPath root version </> "Sapiens4.exe") [])
  {env=Just (filter (\(key,_) -> key `notElem` ["SAPIENS_UPDATE_PREPARING","SAPIENS_UPDATE_ATTEMPT"]) env),close_fds=True}

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = mask $ \restore -> do
 inherited <- getEnvironment
 let environment = [("SAPIENS_UPDATE_PREPARING","1"),("SAPIENS_UPDATE_ATTEMPT",versionName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)]
     command = (proc (versionPath root (target pending) </> "Sapiens4.exe") [])
      {std_in=CreatePipe,std_out=CreatePipe,std_err=CreatePipe,use_process_jobs=True,create_group=True,close_fds=True
      ,env=Just (environment ++ filter (\(key,_) -> key `notElem` map fst environment) inherited)}
 (Just input,Just output,Just errors,child) <- createProcess command
 let job commit = withProcessHandle child $ \state -> case state of
      OpenExtHandle _ handle -> cJob handle (if commit then 1 else 0) >>= check "Candidate process ownership"
      _ -> fail "Candidate has no owned Windows job"
     exited = do
      dead <- withProcessHandle child $ \state -> case state of
       OpenExtHandle handle _ -> (==0) <$> waitProcessHandle handle 0
       _ -> pure True
      if dead then fail "Candidate exited before activation" else threadDelay 100000 >> exited
     await expected = do
      line <- hGetLine output
      when (line == "D2: failed") (fail "Candidate could not prepare the workspace")
      unless (line == expected) (await expected)
     protocol = do
      await "D2: ready"
      state <- readUpdateState root
      unless (state == Pending pending) (fail "Update decision changed before Ready")
      writeUpdateState root (Settled (Selection (target pending) (Just (Previous (current (original pending)) (attempt pending) (backupDigest pending)))))
      job True
      hPutStrLn input "commit" >> hFlush input
      await "D2: active"
     close = mapM_ (\h -> hClose h `catch` \(_::IOException) -> pure ()) [input,output,errors]
 -- Drain bounded chunks rather than retaining potentially private backend logs.
 withAsync (let loop = B.hGetSome errors 4096 >>= \chunk -> unless (B.null chunk) loop in loop) $ \_ ->
  (restore (race_ exited protocol) `catch` \(problem::SomeException) -> do
    state <- readUpdateState root
    case state of
     Settled selected | current selected == target pending -> job True
     _ -> job False >> throwIO problem) `finally` close
