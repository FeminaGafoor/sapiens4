module Update (UpdateBusy(..), updateAndRestart, updateMode, updateAvailable, candidateReady, preparingUpdate, tryUpdate, sendUpdateCommand) where

import Control.Concurrent (Chan, MVar, readChan, withMVar)
import Control.Concurrent.Async (Async, race, waitCatch)
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Either (isRight)
import Data.List (find, stripPrefix)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Unique (hashUnique, newUnique)
import Numeric.Natural (Natural)
import Text.Read (readMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory hiding (executable)
import System.Environment
import System.Exit (exitFailure)
import System.FilePath
import System.IO
import System.Timeout (timeout)
import UpdateState
import UpdatePlatform
import UpdateBackup
import Installation

tryUpdate :: IO a -> IO (Either SomeException a)
tryUpdate = tryJust $ \problem -> case fromException problem :: Maybe SomeAsyncException of
 Just _ -> Nothing
 Nothing -> Just problem

candidateReady :: (String -> IO String) -> IO ()
candidateReady send = do
 preparing <- preparingUpdate
 when preparing $ do
  prepared <- send "prepare-ready"
  unless (prepared == "prepared") (fail "Candidate could not prepare the Workspace")
  putStrLn "update: ready" >> hFlush stdout
  command <- getLine
  unless (command == "commit") (fail "Invalid Coordinator activation message")
  (root,version) <- installed
  state <- readUpdateState root
  unless (case state of Settled s -> current s == version; _ -> False) (fail "Candidate is not durably selected")
  active <- send "activate"
  unless (active == "active") (fail "Candidate could not activate")
  unsetEnv "SAPIENS_UPDATE_ATTEMPT"
  (putStrLn "update: active" >> hFlush stdout) `catch` \(_::IOException) -> pure ()

installed :: IO (FilePath,Version)
installed = installation >>= maybe (fail "Updates require an installed Sapiens version") pure

data ReleaseVersion = ReleaseVersion Natural Natural Natural deriving (Eq, Ord)

data Release = Release
 { releaseVersion :: ReleaseVersion
 , releaseURL :: String
 , releaseDigest :: Sha256
 }

data ReleaseAsset = ReleaseAsset
 { assetName :: String
 , assetURL :: String
 , assetDigest :: Maybe String
 }

data ReleaseFeed = ReleaseFeed T.Text [ReleaseAsset]

instance FromJSON ReleaseFeed where
 parseJSON = withObject "release" $ \o -> ReleaseFeed <$> o .: "tag_name" <*> (o .: "assets" >>= mapM asset)
  where
  asset = withObject "release asset" $ \o ->
   ReleaseAsset <$> o .: "name" <*> o .: "browser_download_url" <*> o .:? "digest"

data ReleaseError
 = InvalidReleaseVersion
 | AlreadyUpToDate
 | MissingPlatformPayload
 | UnexpectedDownloadOrigin
 | InvalidPayloadDigest
 deriving Show

instance Exception ReleaseError where
 displayException InvalidReleaseVersion = "Expected a release version in MAJOR.MINOR.PATCH format"
 displayException AlreadyUpToDate = "Sapiens is already up to date."
 displayException MissingPlatformPayload = "No update payload for this platform is available in the latest release"
 displayException UnexpectedDownloadOrigin = "Unexpected update download origin"
 displayException InvalidPayloadDigest = "Missing or invalid update payload SHA-256"

data UpdateBusy = UpdateBusy deriving Show
instance Exception UpdateBusy where
 displayException _ = "Sapiens is busy. Finish accepted work before updating."

sendUpdateCommand :: MVar () -> Handle -> Chan String -> Async () -> String -> IO String
sendUpdateCommand commandLock input replies reader command = withMVar commandLock $ \_ -> do
 token <- show . hashUnique <$> newUnique
 hPutStrLn input (command ++ " " ++ token)
 hFlush input
 let awaitReply = do
      (received,reply) <- break (==' ') <$> readChan replies
      if received == token then pure (drop 1 reply) else awaitReply
     response = race (waitCatch reader) awaitReply
 outcome <- if command == "prepare-ready" then Just <$> response else timeout 15000000 response
 case outcome of
  Just (Right reply) -> pure reply
  Just (Left _) -> fail "Application server exited during update coordination"
  Nothing -> fail "Application server did not respond to update coordination"

updateAndRestart :: IO () -> (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestart preparing command flushPreferences stop dataDirectory configDirectory = do
 (root,owner) <- installed
 validateRoots root dataDirectory configDirectory
 requireIdle "check-busy" "Sapiens could not check for unfinished work. Update cancelled."
 mask $ \unmask -> do
  next <- freshVersion
  handoffStarted <- newIORef False
  let discardStaged = readIORef handoffStarted >>= \started ->
       unless started (discard [versionPath root next])
  unmask (do
   requestUpdate root owner next
   withCandidate (versionExecutable root owner) ["--update-coordinate",versionName next,dataDirectory,configDirectory] [] $ \coordinator -> mask $ \restoreHandoff -> do
    let quiet = requireIdle "quiesce" "Sapiens could not pause safely. Update cancelled."
    (do
      restoreHandoff $ do
       preparing
       await 15 coordinator "update: coordinator"
       (quiet >> flushPreferences) `onException` void (tryUpdate (command "resume"))
       stop
      candidateOwnership coordinator True)
     `onException` candidateOwnership coordinator False
    writeIORef handoffStarted True
    sendCandidate coordinator "handoff")
   `onException` discardStaged
 where
 requireIdle action message = command action >>= \reply -> case reply of
  "quiet" -> pure ()
  "busy" -> throwIO UpdateBusy
  _ -> fail message

requestUpdate :: FilePath -> Version -> Version -> IO ()
requestUpdate root owner next = flip finally (discard [payload]) $ do
 validatePath (root </> "versions")
 manager <- newManager tlsManagerSettings
 release <- discoverRelease manager root owner
 transfer <- parseRequest (releaseURL release)
 withResponse transfer manager $ \result -> withBinaryFile payload WriteMode $ \file ->
  let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
  in loop
 validatePath payload
 verifyPayloadDigest (releaseDigest release) payload
 stagePayload (versionExecutable root owner) payload (versionPath root next)
 staged <- installedVersion root next
 unless (staged == releaseVersion release) (fail "Update payload version differs from the release tag")
 where payload = root </> (versionName next ++ "-" ++ payloadName)

verifyPayloadDigest :: Sha256 -> FilePath -> IO ()
verifyPayloadDigest expected path = withBinaryFile path ReadMode $ \file -> do
 digest <- sha256 <$> BL.hGetContents file
 unless (digest == expected) (fail "Update payload SHA-256 differs from the release")

updateAvailable :: IO Bool
updateAvailable = fmap isRight $ tryUpdate $ do
 (root,owner) <- installed
 manager <- newManager tlsManagerSettings
 discoverRelease manager root owner

installedVersion :: FilePath -> Version -> IO ReleaseVersion
installedVersion root version =
 readFile (versionPath root version </> "sapiens-release.version") >>= either throwIO pure . parseReleaseVersion . T.pack

discoverRelease :: Manager -> FilePath -> Version -> IO Release
discoverRelease manager root owner = do
 installedRelease <- installedVersion root owner
 request <- parseRequest ("https://api.github.com/repos/" ++ releaseRepository ++ "/releases/latest")
 body <- responseBody <$> httpLbs request {requestHeaders=[("User-Agent","Sapiens4-Updater"),("Accept","application/vnd.github+json")]} manager
 feed <- either fail pure (eitherDecode body)
 either throwIO pure (selectRelease installedRelease payloadName feed)

releaseRepository :: String
releaseRepository = "aleski-green/sapiens4"

selectRelease :: ReleaseVersion -> String -> ReleaseFeed -> Either ReleaseError Release
selectRelease installedRelease platformPayload (ReleaseFeed tag assets) = do
 available <- parseReleaseVersion tag
 unless (available > installedRelease) (Left AlreadyUpToDate)
 asset <- maybe (Left MissingPlatformPayload) Right (find ((== platformPayload) . assetName) assets)
 let expectedURL = "https://github.com/" ++ releaseRepository ++ "/releases/download/" ++ T.unpack tag ++ "/" ++ platformPayload
 unless (assetURL asset == expectedURL) (Left UnexpectedDownloadOrigin)
 case assetDigest asset >>= stripPrefix "sha256:" >>= parseSha256 of
  Just digest -> Right (Release available expectedURL digest)
  _ -> Left InvalidPayloadDigest

parseReleaseVersion :: T.Text -> Either ReleaseError ReleaseVersion
parseReleaseVersion input =
 case traverse component (T.splitOn "." (fromMaybe trimmed (T.stripPrefix "v" trimmed))) of
  Just [major,minor,patch] -> Right (ReleaseVersion major minor patch)
  _ -> Left InvalidReleaseVersion
 where
 trimmed = T.stripEnd input
 component field
  | T.all (\c -> c >= '0' && c <= '9') field = readMaybe (T.unpack field)
  | otherwise = Nothing

updateMode :: [String] -> IO Bool
updateMode args = case args of
 ["--update-coordinate",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseVersion name)
  putStrLn "update: coordinator" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  when (command == "handoff") $ report (coordinate (Just (version,dataDirectory,configDirectory)))
  pure True
 ["--update-recover"] -> report (coordinate Nothing) >> pure True
 _ -> pure False
 where
 report action = tryUpdate action >>= either (\problem -> showUpdateError (displayException problem) >> exitFailure) pure

data Dispatch = Running | Launch Version | Recover

newtype CandidateNotJoined = CandidateNotJoined SomeException deriving Show
instance Exception CandidateNotJoined where
 displayException (CandidateNotJoined problem) =
  "The candidate process tree could not be joined. Recovery is deferred. Close Sapiens and relaunch from its installed shortcut. " ++ displayException problem

discard :: [FilePath] -> IO ()
discard paths = forM_ paths $ \path ->
 (pathPresent path >>= flip when (removePathForcibly path)) `catch` \(_::IOException) -> pure ()

coordinate :: Maybe (Version,FilePath,FilePath) -> IO ()
coordinate requested = do
 (root,owner) <- installed
 dispatch <- withLease (leasePath root) True $ \lease -> do
  state <- readUpdateState root
  case (state,requested) of
   (Pending pending,_) -> do
    unless (owner == current (original pending)) (fail "Recovery must run through retained original version")
    restoreSnapshot root pending (writeUpdateState root (Settled (original pending)))
    discard [versionPath root (target pending), backupPath root (attempt pending)]
    pure (Launch (current (original pending)))
   (Settled selected,Nothing) -> pure (Launch (current selected))
   (Settled selected,Just (next,dataDirectory,configDirectory)) -> do
    unless (owner == current selected && newTarget selected next) (fail "Update selection changed")
    nonce <- freshAttemptId
    let backup = backupPath root nonce
        discardUnpublished = do
         decision <- tryUpdate (readUpdateState root)
         case decision of
          Right (Settled unchanged) | unchanged == selected -> discard [versionPath root next,backup]
          _ -> pure ()
    result <- tryUpdate $ flip onException discardUnpublished $ do
     originalData <- canonicalizePath dataDirectory
     originalConfig <- canonicalizePath configDirectory
     validateRoots root originalData originalConfig
     claimConfigRoot originalConfig
     let backups = root </> "backups"
     exists <- pathPresent backups
     unless exists (createDirectory backups)
     validatePath (versionExecutable root next)
     withRootIdentity originalData $ \dataId -> withRootIdentity originalConfig $ \configId -> do
      saved <- withRootLocks originalData $ do
       fingerprint <- snapshot backup originalData originalConfig
       reserveDataRoot originalData (DataReservation root nonce)
       let pending = PendingUpdate selected next nonce originalData originalConfig dataId configId fingerprint
       writeUpdateState root (Pending pending)
       pure pending
      downgradeLease lease
      runCandidate root saved
    case result of
     Right () -> do
      forM_ (previous selected) $ \old ->
       discard [versionPath root (previousVersion old),backupPath root (previousBackup old)]
      pure Running
     Left problem -> do
      decision <- readUpdateState root
      hPutStrLn stderr ("Sapiens update: " ++ displayException problem) `catch` \(_::IOException) -> pure ()
      case decision of
       Settled selection | current selection == next ->
        fail ("The update was committed, but activation failed. Quit Sapiens, then open it from the installed shortcut to start the selected version. " ++ displayException problem)
       Settled selection -> pure (Launch (current selection))
       Pending _ | Just (_::CandidateNotJoined) <- fromException problem -> throwIO problem
       Pending _ -> pure Recover
 case dispatch of
  Running -> pure ()
  Launch version -> launchVersion root version []
  Recover -> coordinate Nothing

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = withCandidate (versionExecutable root (target pending)) []
 [("SAPIENS_UPDATE_ATTEMPT",attemptName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)] $ \candidate -> do
  (do
   await (60 * 60) candidate "update: ready"
   alive <- candidateAlive candidate
   unless alive (fail "Candidate exited before commit")
   state <- readUpdateState root
   unless (state == Pending pending) (fail "Update decision changed before Ready")
   mask_ $ do
    writeUpdateState root (Settled (Selection (target pending) (Just (Previous (current (original pending)) (attempt pending) (backupDigest pending)))))
    candidateOwnership candidate True)
   `onException` (tryUpdate (candidateOwnership candidate False) >>= either (throwIO . CandidateNotJoined) pure)
  sendCandidate candidate "commit"
  await 60 candidate "update: active"

await :: Int -> Candidate -> String -> IO ()
await seconds candidate expected =
 timeout (seconds * 1000000) loop >>= maybe (fail ("Timed out waiting for " ++ expected)) pure
 where
 loop = do
  message <- candidateMessage candidate
  when (message == "update: failed") (fail ("Candidate failed before " ++ expected))
  unless (message == expected) loop
