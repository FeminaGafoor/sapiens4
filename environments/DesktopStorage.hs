module DesktopStorage (WorkspaceSignal(..), saveWhenReady, awaitServerAddress, storageMode, preferenceScript, validPreference, bootstrapConsentScript, applyBootstrapConsent) where

import Control.Exception (SomeException, IOException, try, displayException)
import Control.Concurrent (forkIO)
import Control.Monad (forM_, guard, void, unless)
import Data.IORef
import Data.Aeson (Value(..), encode, decodeStrict, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import DataRoot (StorageChoice(..), saveDataDirectory, storageError, storageMessage, startupMessage, openingStorage, openedStorage, startupErrorPrefix, configDirectory)
import Data.List (stripPrefix)
import Network.URI
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (statusCode)
import System.IO (Handle, hGetLine, hIsEOF, hPutStrLn, stderr)
import System.FilePath
import System.Timeout (timeout)
import Types (readJSON, get, boolean)

data WorkspaceSignal = Navigated | WorkspaceReady deriving (Eq)

bootstrapConsentScript :: IO Text
bootstrapConsentScript = do
  result <- try $ do
    path <- (</> "telemetry.json") <$> configDirectory
    boolean . (`get` "enabled") <$> readJSON path Null
  let enabled = either (const False) id (result :: Either IOException Bool)
  pure ("window.setTelemetryConsent?.(" <> TE.decodeUtf8 (BL.toStrict (encode enabled)) <> ")")

applyBootstrapConsent :: Text -> Bool -> IO ()
applyBootstrapConsent address enabled = void $ forkIO $ do
  result <- try $ do
    manager <- HTTP.newManager HTTP.defaultManagerSettings
    request <- HTTP.parseRequest (T.unpack address)
    response <- HTTP.httpNoBody request
      { HTTP.path="/api/host/telemetry", HTTP.method="PUT"
      , HTTP.requestHeaders=[("Content-Type","application/json"),("X-Sapiens-Local","1")]
      , HTTP.requestBody=HTTP.RequestBodyLBS (encode (object ["enabled" .= enabled])) } manager
    unless (statusCode (HTTP.responseStatus response)==200) (ioError (userError "Could not save usage reporting preference"))
  forM_ (either Just (const Nothing) (result :: Either SomeException ())) (hPutStrLn stderr . displayException)

saveWhenReady :: IORef (Maybe WorkspaceSignal) -> WorkspaceSignal -> IORef (Maybe a)
              -> (a -> FilePath) -> (String -> IO ()) -> (a -> IO ()) -> IO ()
saveWhenReady first signal pending directory recover opened = do
  previous <- readIORef first
  case previous of
    Nothing -> writeIORef first (Just signal)
    Just seen | seen /= signal -> do
      selection <- readIORef pending
      forM_ selection $ \value -> do
        result <- try (saveDataDirectory (directory value))
        case result of
          Left (e :: SomeException) -> recover ("Could not save your storage selection: " ++ storageMessage e)
          Right () -> writeIORef pending Nothing >> opened value
    _ -> pure ()

storageMode :: StorageChoice -> String
storageMode StartNew = "new"
storageMode _ = "existing"

awaitServerAddress :: Handle -> IO URI
awaitServerAddress output = await False
 where
 await opening = do
  let next = hIsEOF output >>= \ended -> if ended then storageError startupMessage else hGetLine output
  line <- if opening then next else timeout 35000000 next >>= maybe (storageError "The application server did not become ready within 35 seconds.") pure
  case stripPrefix startupErrorPrefix line of
   Just encoded -> storageError (maybe startupMessage id (decodeStrict $ TE.encodeUtf8 $ T.pack encoded))
   Nothing | line==openingStorage -> await True
           | line==openedStorage -> await False
           | otherwise -> maybe (await opening) pure (address line)
 address line = do
  uri <- stripPrefix "Sapiens4: " line >>= parseURI
  auth <- uriAuthority uri
  guard (uriScheme uri=="http:" && uriRegName auth=="127.0.0.1" && null (uriUserInfo auth))
  pure uri

validPreference :: Text -> Text -> Bool
validPreference key value =
  (key == "sapi-color-theme" || key == "s4.active" || "s4.workspace." `T.isPrefixOf` key)
    && T.length key <= 128 && T.length value <= 1000000

preferenceScript :: FilePath -> Text -> FilePath -> Value -> IO Text
preferenceScript runtime address directory saved = do
  source <- TIO.readFile (runtime </> "app/shell/client-preferences.js")
  token <- UUID.nextRandom
  let arguments = TE.decodeUtf8 (BL.toStrict (encode (address, directory, saved, token)))
  pure ("(" <> source <> ")(..." <> arguments <> ");")
