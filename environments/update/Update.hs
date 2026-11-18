module Update (UpdateCache, UpdateBusy(..), withUpdateCache, updateAndRestartCached, updateMode, updateAvailable, candidateReady, preparingUpdate, tryUpdate, sendUpdateCommand) where

import Control.Concurrent (Chan, MVar, readChan, withMVar)
import Control.Concurrent.Async (Async, race, waitCatch)
import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isSpace)
import Data.List (stripPrefix)
import Data.IORef
import Data.Unique (hashUnique, newUnique)
import Text.Read (readMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Numeric (showHex)
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
  command <- getLine `catch` \(_::IOException) -> pure ""
  unless (command `elem` ["commit", ""]) (fail "Invalid Coordinator activation message")
  (root,version) <- installed
  state <- readUpdateState root
  unless (case state of Settled s -> current s == version; _ -> False) (fail "Candidate is not durably selected")
  active <- send "activate"
  unless (active == "active") (fail "Candidate could not activate")
  unsetEnv "SAPIENS_UPDATE_ATTEMPT"
  (putStrLn "update: active" >> hFlush stdout) `catch` \(_::IOException) -> pure ()

installed :: IO (FilePath,Version)
installed = installation >>= maybe (fail "Updates require an installed Sapiens version") pure

data Release = Release
 { releaseVersion :: [Integer]
 , releaseURL :: String
 , releaseDigest :: String
 } deriving Eq
newtype UpdateCache = UpdateCache (IORef (Maybe (Release,FilePath)))

withUpdateCache :: (UpdateCache -> IO a) -> IO a
withUpdateCache = bracket (UpdateCache <$> newIORef Nothing) clearUpdateCache

clearUpdateCache :: UpdateCache -> IO ()
clearUpdateCache (UpdateCache ref) = atomicModifyIORef' ref (\old -> (Nothing,old)) >>= mapM_ (discard . pure . snd)

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

updateAndRestartCached :: UpdateCache -> IO () -> (String -> IO String) -> IO () -> IO () -> FilePath -> FilePath -> IO ()
updateAndRestartCached cache preparing command flushPreferences stop dataDirectory configDirectory = mask $ \restore -> do
 result <- try (restore attempt)
 case result of
  Left problem | Just UpdateBusy <- fromException problem -> throwIO problem
  _ -> clearUpdateCache cache >> either throwIO pure result
 where
 requireIdle action message = command action >>= \reply -> case reply of
  "quiet" -> pure ()
  "busy" -> throwIO UpdateBusy
  _ -> fail message
 attempt = do
  (root,owner) <- installed
  validateRoots root dataDirectory configDirectory
  requireIdle "check-busy" "Sapiens could not check for unfinished work. Update cancelled."
  mask $ \unmask -> do
   next <- freshVersion
   handoffStarted <- newIORef False
   let discardStaged = readIORef handoffStarted >>= \started ->
        unless started (discard [versionPath root next])
   unmask (do
    requestUpdate cache root owner next
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

requestUpdate :: UpdateCache -> FilePath -> Version -> Version -> IO ()
requestUpdate cache@(UpdateCache ref) root owner next = do
 validatePath (root </> "versions")
 manager <- newManager tlsManagerSettings
 release <- discoverRelease manager root owner
 existing <- readIORef ref
 payload <- case existing of
  Just (cached,path) | release == cached -> pure path
  _ -> do
   clearUpdateCache cache
   let path = root </> (versionName next ++ "-" ++ payloadName)
   writeIORef ref (Just (release,path))
   transfer <- parseRequest (releaseURL release)
   withResponse transfer manager $ \result -> withBinaryFile path WriteMode $ \file ->
    let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
    in loop
   pure path
 validatePath payload
 verifyPayloadDigest (releaseDigest release) payload
 stagePayload payload (versionPath root next)
 staged <- installedVersion root next
 unless (staged == releaseVersion release) (fail "Update payload version differs from the release tag")

verifyPayloadDigest :: String -> FilePath -> IO ()
verifyPayloadDigest expected path = do
 digest <- withBinaryFile path ReadMode $ \file ->
  let readChunks context = do
       chunk <- B.hGetSome file 65536
       if B.null chunk then pure (SHA256.finalize context)
        else readChunks (SHA256.update context chunk)
  in readChunks SHA256.init
 let hex byte = let digits = showHex byte "" in replicate (2 - length digits) '0' ++ digits
 unless (concatMap hex (B.unpack digest) == expected) (fail "Update payload SHA-256 differs from the release")

updateAvailable :: IO Bool
updateAvailable = do
 result <- tryUpdate $ do
  (root,owner) <- installed
  manager <- newManager tlsManagerSettings
  void (discoverRelease manager root owner)
 pure (either (const False) (const True) result)

installedVersion :: FilePath -> Version -> IO [Integer]
installedVersion root version = readFile (versionPath root version </> "sapiens-release.version") >>= parseReleaseVersion

discoverRelease :: Manager -> FilePath -> Version -> IO Release
discoverRelease manager root owner = do
 installedRelease <- installedVersion root owner
 let repository = "aleski-green/sapiens4"
 request <- parseRequest ("https://api.github.com/repos/" ++ repository ++ "/releases/latest")
 body <- responseBody <$> httpLbs request {requestHeaders=[("User-Agent","Sapiens4-Updater"),("Accept","application/vnd.github+json")]} manager
 value <- either fail pure (eitherDecode body)
 tag <- either fail pure (parseEither (withObject "release" (\o -> o .: "tag_name")) value)
 available <- parseReleaseVersion tag
 unless (available > installedRelease) (fail "Sapiens is already up to date.")
 assets <- either fail pure (parseEither (withObject "release" (\o -> o .: "assets")) value) :: IO [Value]
 let asset = withObject "release asset" $ \o -> (,) <$> o .: "name" <*> ((,) <$> o .: "browser_download_url" <*> o .:? "digest")
 entries <- forM assets (either fail pure . parseEither asset) :: IO [(String,(String,Maybe String))]
 (url,publishedDigest) <- maybe (fail "No update payload for this platform is available in the latest release") pure (lookup payloadName entries)
 unless (url == "https://github.com/" ++ repository ++ "/releases/download/" ++ tag ++ "/" ++ payloadName) (fail "Unexpected update download origin")
 case publishedDigest >>= stripPrefix "sha256:" of
  Just digest | validDigest digest -> pure (Release available url digest)
  _ -> fail "Missing or invalid update payload SHA-256"

parseReleaseVersion :: String -> IO [Integer]
parseReleaseVersion input = do
 let trimmed = reverse (dropWhile isSpace (reverse input))
     version = case trimmed of 'v':rest -> rest; _ -> trimmed
     parts value = case break (=='.') value of
      (part,'.':rest) -> part : parts rest
      (part,_) -> [part]
     fields = parts version
 let digits field = not (null field) && all (\c -> c >= '0' && c <= '9') field
 case traverse readMaybe fields of
  Just release | length fields == 3 && all digits fields -> pure release
  _ -> fail "Expected a release version in MAJOR.MINOR.PATCH format"


updateMode :: [String] -> IO Bool
updateMode args = case args of
 ["--update-coordinate",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseEither parseJSON (toJSON name))
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
    unless (owner == current selected && next /= owner && Just next /= (previousVersion <$> previous selected)) (fail "Update selection changed")
    nonce <- freshVersion
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
       reserveDataRoot originalData root nonce
       let pending = PendingUpdate selected next nonce originalData originalConfig dataId configId fingerprint
       writeUpdateState root (Pending pending)
       pure pending
      downgradeLease lease
      runCandidate root saved
    case result of
     Right () -> do
      forM_ (previous selected) $ \old ->
       discard [versionPath root (previousVersion old),root </> "backups" </> versionName (previousBackup old)]
      pure Running
     Left problem -> do
      decision <- readUpdateState root
      hPutStrLn stderr ("Sapiens update: " ++ displayException problem) `catch` \(_::IOException) -> pure ()
      case decision of
       Settled selection | current selection == next ->
        fail ("The update was committed, but activation failed. Relaunch Sapiens with its installed shortcut. " ++ displayException problem)
       Settled selection -> pure (Launch (current selection))
       Pending _ | Just (_::CandidateNotJoined) <- fromException problem -> throwIO problem
       Pending _ -> pure Recover
 case dispatch of
  Running -> pure ()
  Launch version -> launchVersion root version []
  Recover -> coordinate Nothing

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = withCandidate (versionExecutable root (target pending)) []
 [("SAPIENS_UPDATE_ATTEMPT",versionName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)] $ \candidate -> do
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
  when (message == "update: failed") (fail "Candidate could not prepare the workspace")
  unless (message == expected) loop
