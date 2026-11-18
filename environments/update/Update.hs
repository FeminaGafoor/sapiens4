{-# LANGUAGE CPP #-}
module Update (updateMode, updateAndRestart, candidateReady, preparingUpdate, tryUpdate) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Aeson
import Data.Aeson.Types (parseEither)
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Directory hiding (executable)
import System.Environment
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import UpdateState
import UpdatePlatform
import UpdateBackup
import Installation

tryUpdate :: IO a -> IO (Either SomeException a)
tryUpdate = tryJust $ \problem -> case fromException problem :: Maybe SomeAsyncException of
 Just _ -> Nothing
 Nothing -> Just problem

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
updateAndRestart command flushPreferences stop dataDirectory configDirectory = do
 (root,owner) <- installed
 validateRoots root dataDirectory configDirectory
 next <- requestUpdate root owner
 withCandidate (versionExecutable root owner) ["--d2-update",versionName next,dataDirectory,configDirectory] [] $ \coordinator ->
  (do
   await coordinator "D2: coordinator"
   let quiet = command "quiesce" >>= \reply -> case reply of
        "quiet" -> pure ()
        "busy" -> fail "Sapiens is busy. Finish accepted work before updating."
        _ -> fail "Sapiens could not pause safely. Update cancelled."
   (do
     (quiet >> flushPreferences) `onException` void (tryUpdate (command "resume"))
     stop)
    `onException` removePathForcibly (versionPath root next)
   candidateOwnership coordinator True
   sendCandidate coordinator "handoff")
  `onException` candidateOwnership coordinator False

requestUpdate :: FilePath -> Version -> IO Version
requestUpdate root owner = do
 nonce <- freshVersion
 let payload = root </> (versionName nonce ++ "-" ++ payloadName)
     destination = versionPath root nonce
     stage = stagePayload payload destination
 validatePath (root </> "versions")
#ifdef D2_FIXTURE
 fixture <- lookupEnv "SAPIENS_D2_FIXTURE_PAYLOAD"
 let prepare = maybe (signedUpdate nonce payload stage) (\file -> copyFile file payload >> stage) fixture
#else
 let prepare = signedUpdate nonce payload stage
#endif
 (prepare `finally` removeIfPresent payload) `onException` removePathForcibly destination
 pure nonce
 where
 removeIfPresent path = removeFile path `catch` \(e::IOException) ->
  unless (isDoesNotExistError e) (throwIO e)
 installedVersion version = readFile (versionPath root version </> "sapiens-release.version") >>= parseReleaseVersion
 publisher = verifyPublisher (versionExecutable root owner)
 signedUpdate nonce path stage = do
  installedRelease <- installedVersion owner
  manager <- newManager tlsManagerSettings
  let repository = "aleski-green/sapiens4"
  request <- parseRequest ("https://api.github.com/repos/" ++ repository ++ "/releases/latest")
  response <- httpLbs request {requestHeaders=[("User-Agent","Sapiens4-Updater"),("Accept","application/vnd.github+json")]} manager
  value <- either fail pure (eitherDecode (responseBody response))
  tag <- either fail pure (parseEither (withObject "release" (\o -> o .: "tag_name")) value)
  available <- parseReleaseVersion tag
  unless (available > installedRelease) (fail "Sapiens is already up to date.")
  assets <- either fail pure (parseEither (withObject "release" (\o -> o .: "assets")) value) :: IO [Value]
  let artifact = payloadName
      asset = withObject "release asset" $ \o -> (,) <$> o .: "name" <*> o .: "browser_download_url"
  urls <- forM assets (either fail pure . parseEither asset) :: IO [(String,String)]
  url <- maybe (fail "No update payload for this platform is available in the latest release") pure (lookup artifact urls)
  unless (("https://github.com/" ++ repository ++ "/releases/download/" ++ tag ++ "/") `isPrefixOf` url) (fail "Unexpected update download origin")
  transfer <- parseRequest url
  withResponse transfer manager $ \result -> withBinaryFile path WriteMode $ \file ->
   let loop = do chunk <- brRead (responseBody result); unless (B.null chunk) (B.hPut file chunk >> loop)
   in loop
  publisher path
  stage
  publisher (versionExecutable root nonce)
  staged <- installedVersion nonce
  unless (staged == available) (fail "Signed payload version differs from the release tag")

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
 ["--d2-update",name,dataDirectory,configDirectory] -> do
  version <- either fail pure (parseEither parseJSON (toJSON name))
  putStrLn "D2: coordinator" >> hFlush stdout
  command <- getLine `catch` \(_::IOException) -> pure ""
  when (command == "handoff") $ do
   result <- tryUpdate (coordinate (Just (version,dataDirectory,configDirectory)))
   either (showUpdateError . displayException) pure result
  pure True
 ["--d2-recover"] -> coordinate Nothing >> pure True
 _ -> pure False

data Dispatch = Running | Launch Version | Recover
discard :: [FilePath] -> IO ()
discard paths = forM_ paths $ \path -> removePathForcibly path `catch` \(_::IOException) -> pure ()

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
    discard [versionPath root (target pending), backupPath root pending]
    pure (Launch (current (original pending)))
   (Settled selected,Nothing) -> pure (Launch (current selected))
   (Settled selected,Just (next,dataDirectory,configDirectory)) -> do
    unless (owner == current selected && next /= owner && Just next /= (previousVersion <$> previous selected)) (fail "Update selection changed")
    result <- tryUpdate $ do
     originalData <- canonicalizePath dataDirectory
     originalConfig <- canonicalizePath configDirectory
     validateRoots root originalData originalConfig
     nonce <- freshVersion
     let backups = root </> "backups"
     exists <- pathPresent backups
     unless exists (createDirectory backups)
     validatePath (versionExecutable root next)
     let pending = PendingUpdate selected next nonce originalData originalConfig ""
     fingerprint <- withRootLocks originalData $ snapshot (backupPath root pending) originalData originalConfig
     let saved = pending {backupDigest=fingerprint}
     writeUpdateState root (Pending saved)
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
      pure $ case decision of
       Settled selection | current selection == next -> Running
       Settled selection -> Launch (current selection)
       Pending _ -> Recover
 case dispatch of
  Running -> pure ()
  Launch version -> launchVersion root version []
  Recover -> coordinate Nothing

runCandidate :: FilePath -> PendingUpdate -> IO ()
runCandidate root pending = withCandidate (versionExecutable root (target pending)) []
 [("SAPIENS_UPDATE_ATTEMPT",versionName (attempt pending)),("SAPIENS_DATA_DIR",dataRoot pending),("SAPIENS_CONFIG_DIR",configRoot pending)] $ \candidate -> do
  (do
   await candidate "D2: ready"
   alive <- candidateAlive candidate
   unless alive (fail "Candidate exited before commit")
   state <- readUpdateState root
   unless (state == Pending pending) (fail "Update decision changed before Ready")
   mask_ $ do
    writeUpdateState root (Settled (Selection (target pending) (Just (Previous (current (original pending)) (attempt pending) (backupDigest pending)))))
    candidateOwnership candidate True)
   `onException` candidateOwnership candidate False
  sendCandidate candidate "commit"
  await candidate "D2: active"

await :: Candidate -> String -> IO ()
await candidate expected = do
 message <- candidateMessage candidate
 when (message == "D2: failed") (fail "Candidate could not prepare the workspace")
 unless (message == expected) (await candidate expected)
