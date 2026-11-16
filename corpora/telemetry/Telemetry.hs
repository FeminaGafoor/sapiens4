module Telemetry
 ( Telemetry, RuntimeKind(..), Event(..), openTelemetry, openTelemetryWith, closeTelemetry
 , enabled, setConsent, consentView, emit, emitAt, emitCorporaAt, heartbeatInterval, uiHeartbeat
 ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Version (showVersion)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Paths_sapiens_domain (version)
import RuntimePaths (configDirectory)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.Info (os,arch)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import TelemetryWire

data RuntimeKind = Desktop | Headless deriving Eq
data Event = AppOpened | CorporaCreated | RequestAccepted | RemotePaired | SchedulerHeartbeat | UIHeartbeat
 deriving Eq
eventName :: Event -> Text
eventName event = case event of
 AppOpened -> "app_opened"
 CorporaCreated -> "corpora_created"
 RequestAccepted -> "request_accepted"
 RemotePaired -> "remote_paired"
 SchedulerHeartbeat -> "scheduler_heartbeat"
 UIHeartbeat -> "ui_heartbeat"

queueLimit, batchDelayMicros, requestTimeoutMicros :: Int
queueLimit = 100
batchDelayMicros = 10 * 1000000
requestTimeoutMicros = 10 * 1000000
heartbeatInterval :: NominalDiffTime
heartbeatInterval = 60 * 60

defaultTelemetryOrigin :: String
defaultTelemetryOrigin = "https://telemetry.sapiens4.ai"

batchEnvelope :: Telemetry -> [Value] -> Value
batchEnvelope telemetry batch = object ["context" .= hostContext telemetry,"events" .= batch]

data Saved = Saved Bool (Maybe Credentials)
instance ToJSON Saved where
 toJSON (Saved on credentials) = case toJSON credentials of
  Object fields -> Object (KM.insert "enabled" (Bool on) fields)
  _ -> object ["enabled" .= on]
instance FromJSON Saved where
 parseJSON value = withObject "telemetry settings" (\o -> do
  on <- o .: "enabled"
  credentials <- if KM.member "installation_id" o || KM.member "password" o then Just <$> parseJSON value else pure Nothing
  pure (Saved on credentials)) value

data SenderStatus = Running | Disabled Text | Closed deriving Eq
data Pending = Pending { pendingAt :: Integer, pendingValue :: Value, pendingCorpora :: Maybe (IO Text) }
data Telemetry = Telemetry
 { savedSettings :: MVar Saved, settingsPath :: Maybe FilePath, senderStatus :: TVar SenderStatus
 , consent :: TVar Bool, pendingEvents :: TVar (Seq.Seq Pending), senderThread :: Async ()
 , hostContext :: Value, origin :: Maybe String, manager :: Manager }

readSaved :: FilePath -> IO Saved
readSaved path = do
 result <- try (BL.readFile path)
 case result of
  Left e | isDoesNotExistError e -> pure (Saved False Nothing)
  Left e -> throwIO (e :: IOException)
  Right raw -> either (ioError . userError) pure (eitherDecode raw)

writeSaved :: FilePath -> Saved -> IO ()
writeSaved path value = do
 createDirectoryIfMissing True (takeDirectory path)
 bracketOnError (openBinaryTempFile (takeDirectory path) ".telemetry.tmp")
  (\(temporary,handle) -> hClose handle `finally` removeFile temporary) $ \(temporary,handle) -> do
   BL.hPut handle (encode value)
   hClose handle
   renameFile temporary path

openTelemetry :: RuntimeKind -> IO Telemetry
openTelemetry runtime = newManager tlsManagerSettings >>= \http -> openTelemetryWith http runtime

openTelemetryWith :: Manager -> RuntimeKind -> IO Telemetry
openTelemetryWith http runtime = do
 loaded <- try $ do
  path <- (</> "telemetry.json") <$> configDirectory
  value <- readSaved path
  endpoint <- maybe defaultTelemetryOrigin id <$> lookupEnv "SAPIENS_TELEMETRY_ORIGIN"
  pure (path,value,endpoint)
 let (path,initial,status,endpoint) = case loaded of
      Left (e::IOException) -> (Nothing,Saved False Nothing,Disabled (T.pack (displayException e)),Nothing)
      Right (file,value,endpoint) -> (Just file,value,Running,Just endpoint)
     Saved on _ = initial
 saved <- newMVar initial
 active <- newTVarIO on
 state <- newTVarIO status
 queue <- newTVarIO Seq.empty
 let context = object ["app_version" .= showVersion version,"host_os" .= os,"host_arch" .= arch,"runtime_kind" .= (if runtime==Desktop then "desktop" else "headless" :: Text)]
 holder <- newEmptyMVar
 worker <- async (readMVar holder >>= sender)
 let telemetry = Telemetry saved path state active queue worker context endpoint http
 putMVar holder telemetry
 pure telemetry

closeTelemetry :: Telemetry -> IO ()
closeTelemetry telemetry = do
 atomically $ writeTVar (senderStatus telemetry) Closed >> writeTVar (consent telemetry) False >> writeTVar (pendingEvents telemetry) Seq.empty
 void $ async (cancel (senderThread telemetry))

enabled :: Telemetry -> IO Bool
enabled telemetry = atomically $ (&&) <$> readTVar (consent telemetry) <*> ((==Running) <$> readTVar (senderStatus telemetry))

disable :: Telemetry -> Text -> IO ()
disable telemetry reason = atomically $ writeTVar (senderStatus telemetry) (Disabled reason) >> writeTVar (pendingEvents telemetry) Seq.empty

refreshConsent :: Telemetry -> IO Bool
refreshConsent telemetry = do
 result <- try $ modifyMVar_ (savedSettings telemetry) $ \_ -> do
  file <- maybe (fail "Telemetry configuration unavailable") pure (settingsPath telemetry)
  latest@(Saved on _) <- readSaved file
  unless on $ atomically $ writeTVar (consent telemetry) False >> writeTVar (pendingEvents telemetry) Seq.empty
  pure latest
 case result of
  Left (e::IOException) -> disable telemetry (T.pack (displayException e)) >> pure False
  Right () -> enabled telemetry

setConsent :: Telemetry -> Bool -> IO (Either Text ())
setConsent telemetry on = do
 unless on $ atomically $ writeTVar (consent telemetry) False >> writeTVar (pendingEvents telemetry) Seq.empty
 result <- try $ modifyMVar_ (savedSettings telemetry) $ \(Saved _ credentials) -> do
  path <- maybe (ioError (userError "Telemetry configuration is unavailable until restart")) pure (settingsPath telemetry)
  let updated = Saved on credentials
  writeSaved path updated
  pure updated
 case result of
  Left (e::IOException) -> do
   let reason = T.pack (displayException e)
   disable telemetry reason
   pure (Left reason)
  Right () -> do
   when on (atomically (writeTVar (consent telemetry) True))
   pure (Right ())

consentView :: Telemetry -> IO Value
consentView telemetry = do
 on <- readTVarIO (consent telemetry)
 status <- readTVarIO (senderStatus telemetry)
 pure $ object ["enabled" .= on,"available" .= (status==Running),"heartbeat_interval_ms" .= (realToFrac heartbeatInterval * 1000 :: Double),"error" .= case status of Disabled reason -> reason; _ -> ""]

emit :: Telemetry -> Event -> Maybe Text -> Object -> IO ()
emit telemetry event corpora fields = getCurrentTime >>= \time -> emitAt telemetry time event corpora fields

emitAt :: Telemetry -> UTCTime -> Event -> Maybe Text -> Object -> IO ()
emitAt telemetry time event corpora = capture telemetry time event (pure <$> corpora)

emitCorporaAt :: Telemetry -> UTCTime -> Event -> IO Text -> Object -> IO ()
emitCorporaAt telemetry time event corpora = capture telemetry time event (Just corpora)

capture :: Telemetry -> UTCTime -> Event -> Maybe (IO Text) -> Object -> IO ()
capture telemetry time event corpora fields = do
 on <- enabled telemetry
 when on $ do
  ident <- UUID.toText <$> UUID.nextRandom
  tick <- toInteger <$> getMonotonicTimeNSec
  let common = KM.fromList [("event_id",toJSON ident),("event_type",toJSON (eventName event)),("occurred_at",toJSON time)]
      row = Object $ KM.union common $ KM.union (maybe KM.empty (const (KM.singleton "corpora_id" (toJSON (UUID.toText UUID.nil)))) corpora) fields
  atomically $ do
   active <- readTVar (consent telemetry)
   status <- readTVar (senderStatus telemetry)
   queue <- readTVar (pendingEvents telemetry)
   when (active && status==Running && Seq.length queue<queueLimit && BL.length (encode (batchEnvelope telemetry [row]))<=fromIntegral requestByteLimit) (writeTVar (pendingEvents telemetry) (queue Seq.|> Pending tick row corpora))

uiHeartbeat :: Telemetry -> Value -> IO ()
uiHeartbeat telemetry value = case value of
 Object fields | KM.lookup "ui_visible" fields==Just (Bool True) && BL.length (encode value)<=8192 -> do
  time <- case KM.lookup "occurred_at" fields >>= \v -> case fromJSON v of Success time -> Just time; Error _ -> Nothing of
   Just time -> pure time
   Nothing -> getCurrentTime
  emitAt telemetry time UIHeartbeat Nothing (KM.filterWithKey (\key _ -> key `elem` allowed) fields)
 _ -> pure ()
 where allowed = ["client_user_agent","client_kind","client_device_family","client_os","client_os_version_ua","client_touch","ui_mode","ui_visible","ui_width","ui_height"]

nextBatch :: Telemetry -> IO [Value]
nextBatch telemetry = do
 first <- atomically $ do
  status <- readTVar (senderStatus telemetry)
  check (status/=Closed)
  active <- readTVar (consent telemetry)
  queue <- readTVar (pendingEvents telemetry)
  check (active && status==Running && not (Seq.null queue))
  pure (Seq.index queue 0)
 now <- toInteger <$> getMonotonicTimeNSec
 timer <- registerDelay (fromInteger (max 0 (toInteger batchDelayMicros - (now-pendingAt first) `div` 1000)))
 batch <- atomically $ do
  queue <- readTVar (pendingEvents telemetry)
  active <- readTVar (consent telemetry)
  status <- readTVar (senderStatus telemetry)
  elapsed <- readTVar timer
  if not active || status/=Running || Seq.null queue || pendingAt (Seq.index queue 0)/=pendingAt first then pure Nothing
  else do
   check (Seq.length queue>=batchLimit || elapsed)
   let fitting prefix [] = prefix
       fitting prefix (event:rest)
        | BL.length (encode (batchEnvelope telemetry (map pendingValue (prefix++[event]))))<=fromIntegral requestByteLimit = fitting (prefix++[event]) rest
        | otherwise = prefix
       count = length (fitting [] (toList (Seq.take batchLimit queue)))
       (taken,rest) = Seq.splitAt count queue
   writeTVar (pendingEvents telemetry) rest
   pure (Just (toList taken))
 case batch of
  Nothing -> nextBatch telemetry
  Just pending -> forM pending $ \event -> case (pendingCorpora event,pendingValue event) of
   (Just identity,Object fields) -> do ident<-identity; pure (Object (KM.insert "corpora_id" (String ident) fields))
   _ -> pure (pendingValue event)

data Delivery = Unavailable | Rejected | Received Value
post :: Telemetry -> String -> Maybe Credentials -> Value -> IO Delivery
post telemetry route credentials value = case origin telemetry of
 Nothing -> pure Unavailable
 Just endpoint -> do
  request <- parseRequest endpoint
  unless (secure request && B.null (queryString request) && path request=="/") (fail "Telemetry requires an HTTPS origin")
  let authorized = maybe id (\(Credentials ident password) -> applyBasicAuth (TE.encodeUtf8 ident) (TE.encodeUtf8 password)) credentials
      outgoing = authorized request {path=BC.pack route,method="POST",requestBody=RequestBodyLBS (encode value),requestHeaders=[("Content-Type","application/json")],responseTimeout=responseTimeoutMicro requestTimeoutMicros,redirectCount=0,checkResponse = \_ _ -> pure ()}
  active <- refreshConsent telemetry
  if not active then pure Unavailable else do
   result <- timeout requestTimeoutMicros $ withResponse outgoing (manager telemetry) $ \response -> do
    body <- brReadSome (responseBody response) (fromIntegral requestByteLimit)
    let code = statusCode (responseStatus response)
    pure $ if code `elem` [401,403] then Rejected else if code>=200 && code<300 then Received (maybe Null id (decode body)) else Unavailable
   pure (maybe Unavailable id result)

credentialsFor :: Telemetry -> IO (Maybe Credentials)
credentialsFor telemetry = do
 Saved _ current <- readMVar (savedSettings telemetry)
 case current of
  Just credentials -> pure (Just credentials)
  Nothing -> do
   enrollment <- post telemetry enrollPath Nothing (object [])
   case enrollment of
    Rejected -> disable telemetry "Telemetry enrollment was rejected until restart" >> pure Nothing
    Unavailable -> pure Nothing
    Received value -> case fromJSON value of
     Error _ -> pure Nothing
     Success credentials -> do
      result <- try $ modifyMVar (savedSettings telemetry) $ \_ -> do
       path <- maybe (fail "Telemetry configuration unavailable") pure (settingsPath telemetry)
       Saved on existing <- readSaved path
       let updated = Saved on (existing `mplus` Just credentials)
       writeSaved path updated
       pure (updated,existing `mplus` Just credentials)
      case result of
       Left (e::IOException) -> disable telemetry (T.pack (displayException e)) >> pure Nothing
       Right installed -> pure installed

sender :: Telemetry -> IO ()
sender telemetry = forever $ do
 (do batch <- nextBatch telemetry
     active <- refreshConsent telemetry
     credentials <- if active then credentialsFor telemetry else pure Nothing
     forM_ credentials $ \auth -> do
      result <- post telemetry eventsPath (Just auth) (batchEnvelope telemetry batch)
      when (case result of Rejected -> True; _ -> False) (disable telemetry "Telemetry credentials or version were rejected until restart"))
  `catch` \(e::SomeException) -> case fromException e :: Maybe SomeAsyncException of Just _ -> throwIO e; Nothing -> pure ()
