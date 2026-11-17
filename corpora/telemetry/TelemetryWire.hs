module TelemetryWire where

import Control.Monad (unless)
import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import qualified Data.UUID as UUID
import qualified Data.Vector as V

enrollPath, eventsPath :: String
enrollPath = "/enroll"
eventsPath = "/events"

batchLimit, requestByteLimit, objectFieldLimit, fieldNameLimit :: Int
batchLimit = 10
requestByteLimit = 65536
objectFieldLimit = 64
fieldNameLimit = 128

data Credentials = Credentials { installationId :: Text, installationPassword :: Text }
 deriving (Eq, Show)
instance ToJSON Credentials where
 toJSON (Credentials ident password) = object ["installation_id" .= ident,"password" .= password]
instance FromJSON Credentials where
 parseJSON = withObject "installation credentials" $ \o -> do
  ident <- o .: "installation_id" >>= uuidText
  password <- o .: "password"
  unless (T.length password == 64 && T.all (`elem` ("0123456789abcdef" :: String)) password) (fail "Invalid installation password")
  pure (Credentials ident password)

uuidText :: Text -> Parser Text
uuidText ident = maybe (fail "Expected a UUID") (pure . UUID.toText) (UUID.fromText ident)

boundedObject :: Value -> Parser Object
boundedObject = withObject "bounded object" $ \o -> do
 unless (KM.size o <= objectFieldLimit && all (\k -> let t=K.toText k in not (T.null t) && T.length t<=fieldNameLimit) (KM.keys o)) (fail "Too many fields or invalid field name")
 pure o

envelope :: Value -> Parser (Object, [Object])
envelope = withObject "telemetry envelope" $ \o -> do
 context <- o .: "context" >>= boundedObject
 mapM_ (\key -> do value <- context .: key; unless (not (T.null value) && T.length value<=200) (fail "Invalid context field")) ["app_version","host_os","host_arch","runtime_kind"]
 events <- o .: "events"
 unless (not (V.null events) && V.length events<=batchLimit) (fail "Expected 1..10 events")
 rows <- mapM event (V.toList events)
 pure (context,rows)
 where
 event value = do
  row <- boundedObject value
  _ <- row .: "event_id" >>= uuidText
  kind <- row .: "event_type"
  unless (not (T.null kind) && T.length kind<=200) (fail "Invalid event_type")
  _ <- row .: "occurred_at" :: Parser UTCTime
  pure row
