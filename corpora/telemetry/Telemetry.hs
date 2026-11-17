module Telemetry
 ( Telemetry, RuntimeKind(..), Event(..), openTelemetry, openTelemetryWith, closeTelemetry
 , enabled, setConsent, consentView, emit, emitAt, heartbeatInterval, uiHeartbeat
 , startupConsentOption, startupConsentArguments
 , RequestTransport(..), requestMetadata, ClientContext, parseClientContext, clientContextFields
 , localClientContext, nativeClientContext
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
import System.Info (arch)
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Timeout (timeout)
import TelemetryWire
import TelemetryDevice

data RuntimeKind = Desktop | Headless deriving Eq
data RequestTransport = Direct | Remote | UnknownTransport
data Event = AppStarted | CorporaCreated | RequestAccepted | RemotePaired | AppHeartbeat | UIHeartbeat
 deriving Eq
eventName :: Event -> Text
eventName event = case event of
 AppStarted -> "app_started"
 CorporaCreated -> "corpora_created"
 RequestAccepted -> "request_accepted"
 RemotePaired -> "remote_paired"
 AppHeartbeat -> "app_heartbeat"
 UIHeartbeat -> "ui_heartbeat"

metadataValue :: Key -> Value -> Value
metadataValue key (Object fields) = maybe Null id (KM.lookup key fields)
metadataValue _ _ = Null

category :: [Text] -> Value -> Text
category allowed (String value) | value `elem` allowed = value
category _ _ = "unknown"

data ClientContext = ClientContext Text Text Text (Maybe DeviceIdentity)
instance ToJSON ClientContext where
 toJSON = Object . clientContextFields . Just

parseClientContext :: Value -> Maybe ClientContext
parseClientContext value@(Object _) = Just (ClientContext kind family platform identity)
 where
 kind = category ["native","browser","installed_webapp"] (metadataValue "client_kind" value)
 family = category ["desktop","phone","tablet"] (metadataValue "client_device_family" value)
 platform = category clientOSCategories (metadataValue "client_os" value)
 identity = if kind=="native" then parseDeviceIdentity (metadataValue "source_device_id" value) (metadataValue "source_device_id_source" value) else Nothing
parseClientContext _ = Nothing

clientContextFields :: Maybe ClientContext -> Object
clientContextFields context = case context of
 Nothing -> clientContextFields (Just (ClientContext "unknown" "unknown" "unknown" Nothing))
 Just (ClientContext kind family platform identity) -> KM.union (deviceFields "source_" identity) (KM.fromList
  [("client_kind",String kind),("client_device_family",String family),("client_os",String platform)])

clientOSCategories :: [Text]
clientOSCategories = ["windows","macos","linux","android","ios","ipados"]

requestMetadata :: RequestTransport -> Maybe ClientContext -> Object
requestMetadata transport value = KM.insert "request_transport" (String name) (clientContextFields value)
 where name = case transport of Direct -> "direct"; Remote -> "remote"; UnknownTransport -> "unknown"

queueLimit, batchDelayMicros, requestTimeoutMicros, deviceContextReserveBytes :: Int
queueLimit = 100
batchDelayMicros = 10 * 1000000
requestTimeoutMicros = 10 * 1000000
deviceContextReserveBytes = 256
heartbeatInterval :: NominalDiffTime
heartbeatInterval = 60 * 60

defaultTelemetryOrigin :: String
defaultTelemetryOrigin = "https://telemetry.sapiens4.ai"

startupConsentOption :: String
startupConsentOption = "--telemetry-consent"

startupConsentArguments :: Maybe Bool -> [String]
startupConsentArguments = maybe [] (\choice -> [startupConsentOption,BC.unpack (BL.toStrict (encode choice))])

batchEnvelope :: Value -> [Value] -> Value
batchEnvelope context batch = object ["context" .= context,"events" .= batch]

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
data Pending = Pending { pendingAt :: Integer, pendingValue :: Value }
data Telemetry = Telemetry
 { savedSettings :: MVar Saved, settingsPath :: Maybe FilePath, senderStatus :: TVar SenderStatus
 , consent :: TVar Bool, pendingEvents :: TVar (Seq.Seq Pending), senderThread :: Async ()
 , hostContext :: Value, nativeDevice :: TVar (Maybe (Maybe DeviceIdentity)), origin :: Maybe Request, manager :: IO Manager }

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

openTelemetry :: RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetry = openTelemetryUsing (newManager tlsManagerSettings)

openTelemetryWith :: Manager -> RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetryWith http = openTelemetryUsing (pure http)

openTelemetryUsing :: IO Manager -> RuntimeKind -> Maybe Bool -> IO Telemetry
openTelemetryUsing create runtime choice = do
 loaded <- try $ do
  path <- (</> "telemetry.json") <$> configDirectory
  value <- readSaved path
  endpoint <- maybe defaultTelemetryOrigin id <$> lookupEnv "SAPIENS_TELEMETRY_ORIGIN"
  pure (path,value,endpoint)
 let (file,initial,loadedStatus,endpoint) = case loaded of
      Left (e::IOException) -> (Nothing,Saved False Nothing,Disabled (T.pack (displayException e)),Nothing)
      Right (settings,value,url) -> (Just settings,value,Running,Just url)
     Saved on _ = initial
 parsed <- try $ forM endpoint $ \url -> do
  request <- parseRequest url `catch` \(e::HttpException) -> ioError (userError (displayException e))
  unless (secure request && B.null (queryString request) && path request=="/") (fail "Telemetry requires an HTTPS origin")
  pure request
 let (request,status) = case parsed of
      Left (e::IOException) -> (Nothing,Disabled (T.pack (displayException e)))
      Right value -> (value,loadedStatus)
 saved <- newMVar initial
 active <- newTVarIO (maybe on (const False) choice)
 state <- newTVarIO status
 queue <- newTVarIO Seq.empty
 device <- newTVarIO Nothing
 connection <- newMVar Nothing
 let http = modifyMVar connection $ \cached -> do
      value <- maybe create pure cached
      pure (Just value,value)
 let context = object ["app_version" .= showVersion version,"host_os" .= nativeOS,"host_arch" .= arch,"runtime_kind" .= (if runtime==Desktop then "desktop" else "headless" :: Text)]
 holder <- newEmptyMVar
 bracketOnError (asyncWithUnmask (\unmask -> unmask (readMVar holder >>= sender))) cancel $ \worker -> do
  let telemetry = Telemetry saved file state active queue worker context device request http
  forM_ choice (void . setConsent telemetry)
  putMVar holder telemetry
  pure telemetry

closeTelemetry :: Telemetry -> IO ()
closeTelemetry telemetry = do
 atomically $ writeTVar (senderStatus telemetry) Closed >> writeTVar (consent telemetry) False >> writeTVar (pendingEvents telemetry) Seq.empty
 cancel (senderThread telemetry)

enabled :: Telemetry -> IO Bool
enabled = atomically . reporting

reporting :: Telemetry -> STM Bool
reporting telemetry = (&&) <$> readTVar (consent telemetry) <*> ((==Running) <$> readTVar (senderStatus telemetry))

cachedNativeDevice :: Telemetry -> IO (Maybe DeviceIdentity)
cachedNativeDevice telemetry = atomically $ do
 active <- reporting telemetry
 identity <- readTVar (nativeDevice telemetry)
 pure (if active then identity >>= id else Nothing)

nativeClientContext :: Telemetry -> IO (Maybe ClientContext)
nativeClientContext telemetry = atomically $ do
 active <- reporting telemetry
 identity <- readTVar (nativeDevice telemetry)
 pure (if active then Just (ClientContext "native" "desktop" nativeOS (identity >>= id)) else Nothing)

localClientContext :: Telemetry -> Value -> IO (Maybe ClientContext)
localClientContext telemetry value = case parseClientContext value of
 Just (ClientContext "native" _ _ _) -> nativeClientContext telemetry
 context -> do
  active <- enabled telemetry
  pure (if active then context else Nothing)

batchContext :: Telemetry -> IO Value
batchContext telemetry = do
 identity <- cachedNativeDevice telemetry
 pure $ case hostContext telemetry of
  Object fields -> Object (KM.union (deviceFields "host_" identity) fields)
  value -> value

disable :: Telemetry -> Text -> IO ()
disable telemetry reason = atomically $ do
 modifyTVar' (senderStatus telemetry) (\status -> if status==Closed then Closed else Disabled reason)
 writeTVar (pendingEvents telemetry) Seq.empty

setConsent :: Telemetry -> Bool -> IO (Either Text ())
setConsent telemetry on = do
 result <- try $ modifyMVarMasked_ (savedSettings telemetry) $ \(Saved _ credentials) -> do
  unless on $ atomically $ writeTVar (consent telemetry) False >> writeTVar (pendingEvents telemetry) Seq.empty
  path <- maybe (ioError (userError "Telemetry configuration is unavailable until restart")) pure (settingsPath telemetry)
  let updated = Saved on credentials
  writeSaved path updated
  atomically $ do
   status <- readTVar (senderStatus telemetry)
   writeTVar (consent telemetry) (on && status/=Closed)
  pure updated
 case result of
  Left (e::IOException) -> do
   let reason = T.pack (displayException e)
   disable telemetry reason
   pure (Left reason)
  Right () -> pure (Right ())

consentView :: Telemetry -> IO Value
consentView telemetry = atomically $ do
 on <- readTVar (consent telemetry)
 status <- readTVar (senderStatus telemetry)
 pure $ object ["enabled" .= on,"available" .= (status==Running),"heartbeat_interval_ms" .= (realToFrac heartbeatInterval * 1000 :: Double),"error" .= case status of Disabled reason -> reason; _ -> ""]

emit :: Telemetry -> Event -> Maybe Text -> Object -> IO ()
emit telemetry event corpora fields = getCurrentTime >>= \time -> emitAt telemetry time event corpora fields

emitAt :: Telemetry -> UTCTime -> Event -> Maybe Text -> Object -> IO ()
emitAt telemetry time event corpora fields = do
 on <- enabled telemetry
 when on $ do
  ident <- UUID.toText <$> UUID.nextRandom
  tick <- toInteger <$> getMonotonicTimeNSec
  let common = KM.fromList [("event_id",toJSON ident),("event_type",toJSON (eventName event)),("occurred_at",toJSON time)]
      row = Object $ KM.union common $ KM.union (maybe KM.empty (KM.singleton "corpora_id" . String) corpora) fields
      limit = (requestByteLimit - fromIntegral (BL.length (encode (batchEnvelope (hostContext telemetry) []))) - deviceContextReserveBytes - batchLimit) `div` batchLimit
  when (BL.length (encode row)<=fromIntegral limit) $ atomically $ do
   active <- reporting telemetry
   queue <- readTVar (pendingEvents telemetry)
   when (active && Seq.length queue<queueLimit) (writeTVar (pendingEvents telemetry) (queue Seq.|> Pending tick row))

uiHeartbeat :: Telemetry -> Maybe Text -> Maybe ClientContext -> Value -> IO ()
uiHeartbeat telemetry corpora context value = case value of
 Object fields | KM.lookup "ui_visible" fields==Just (Bool True) && BL.length (encode value)<=8192 -> do
  time <- case KM.lookup "occurred_at" fields >>= \v -> case fromJSON v of Success time -> Just time; Error _ -> Nothing of
   Just time -> pure time
   Nothing -> getCurrentTime
  let common = clientContextFields context
      selected = case metadataValue "corpora_id" value of String identity -> UUID.toText <$> UUID.fromText identity; _ -> Nothing
      categories = KM.fromList [("ui_mode",String (category ["local","remote_control"] (metadataValue "ui_mode" value))),("target_location",String (category ["local","remote"] (metadataValue "target_location" value)))]
  emitAt telemetry time UIHeartbeat (corpora `mplus` selected) (KM.union common categories)
 _ -> pure ()

nextBatch :: Telemetry -> IO [Value]
nextBatch telemetry = do
 first <- atomically $ do
  active <- reporting telemetry
  queue <- readTVar (pendingEvents telemetry)
  check (active && not (Seq.null queue))
  pure (Seq.index queue 0)
 now <- toInteger <$> getMonotonicTimeNSec
 timer <- registerDelay (fromInteger (max 0 (toInteger batchDelayMicros - (now-pendingAt first) `div` 1000)))
 batch <- atomically $ do
  queue <- readTVar (pendingEvents telemetry)
  active <- reporting telemetry
  elapsed <- readTVar timer
  if not active || Seq.null queue || pendingAt (Seq.index queue 0)/=pendingAt first then pure Nothing
  else do
   check (Seq.length queue>=batchLimit || elapsed)
   let (taken,rest) = Seq.splitAt batchLimit queue
   writeTVar (pendingEvents telemetry) rest
   pure (Just (map pendingValue (toList taken)))
 maybe (nextBatch telemetry) pure batch

data Delivery = Unavailable | Rejected | Received Value
post :: Telemetry -> String -> Maybe Credentials -> Value -> IO Delivery
post telemetry route credentials value = case origin telemetry of
 Nothing -> pure Unavailable
 Just request -> do
  let authorized = maybe id (\(Credentials ident password) -> applyBasicAuth (TE.encodeUtf8 ident) (TE.encodeUtf8 password)) credentials
      outgoing = authorized request {path=BC.pack route,method="POST",requestBody=RequestBodyLBS (encode value),requestHeaders=[("Content-Type","application/json")],responseTimeout=responseTimeoutMicro requestTimeoutMicros,redirectCount=0,checkResponse = \_ _ -> pure ()}
  active <- enabled telemetry
  if not active then pure Unavailable else do
   result <- timeout requestTimeoutMicros $ (do
    http <- manager telemetry
    withResponse outgoing http $ \response -> do
     body <- brReadSome (responseBody response) (fromIntegral requestByteLimit)
     let code = statusCode (responseStatus response)
     pure $ if code `elem` [401,403] then Rejected else if code>=200 && code<300 then Received (maybe Null id (decode body)) else Unavailable)
    `catch` \(_::HttpException) -> pure Unavailable
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
      result <- try $ modifyMVar (savedSettings telemetry) $ \(Saved on existing) -> do
       path <- maybe (fail "Telemetry configuration unavailable") pure (settingsPath telemetry)
       let updated = Saved on (existing `mplus` Just credentials)
       writeSaved path updated
       pure (updated,existing `mplus` Just credentials)
      case result of
       Left (e::IOException) -> disable telemetry (T.pack (displayException e)) >> pure Nothing
       Right installed -> pure installed

sender :: Telemetry -> IO ()
sender telemetry = (forever $ do
  atomically $ reporting telemetry >>= check
  permitted <- enabled telemetry
  cached <- readTVarIO (nativeDevice telemetry)
  when (permitted && cached==Nothing) $ readNativeDevice >>= atomically . writeTVar (nativeDevice telemetry) . Just
  batch <- nextBatch telemetry
  active <- enabled telemetry
  credentials <- if active then credentialsFor telemetry else pure Nothing
  forM_ credentials $ \auth -> do
   context <- batchContext telemetry
   result <- post telemetry eventsPath (Just auth) (batchEnvelope context batch)
   when (case result of Rejected -> True; _ -> False) (disable telemetry "Telemetry credentials or version were rejected until restart"))
 `catch` \(e::SomeException) -> case fromException e :: Maybe SomeAsyncException of
  Just _ -> throwIO e
  Nothing -> do
   let reason = "Telemetry sender failed: "<>T.pack (displayException e)
   disable telemetry reason
   hPutStrLn stderr (T.unpack reason)
