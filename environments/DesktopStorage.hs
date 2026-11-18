module DesktopStorage (WorkspaceSignal(..), StorageAction(..), storageAction, saveWhenReady, awaitServerAddress, storageMode, preferenceScript, validPreference, bootstrapTermsScript, termsVersion, termsAccepted, acceptTerms, requireTerms, startupTelemetryArguments, legalDocument) where

import Control.Exception (SomeException, IOException, try)
import Control.Monad (forM_, guard, unless)
import Data.IORef
import Data.Aeson (Value(..), encode, decodeStrict, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.UUID.V4 as UUID
import DataRoot (StorageChoice(..), saveDataDirectory, storageError, storageMessage, startupMessage, openingStorage, openedStorage, startupErrorPrefix, configDirectory, containedPath)
import Data.List (stripPrefix)
import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.URI
import System.IO (Handle, hGetLine, hIsEOF)
import System.Directory (doesFileExist)
import System.FilePath
import System.Timeout (timeout)
import Types (readJSON, writeJSON, get)
import Telemetry (startupConsentOption)

data WorkspaceSignal = Navigated | WorkspaceReady deriving (Eq)
data StorageAction = StartFresh | ChooseFolder | UseFolder FilePath

storageAction :: Value -> Maybe StorageAction
storageAction input = case get input "storage" of
  String "new" -> Just StartFresh
  String "pick" -> Just ChooseFolder
  String "existing" -> Just (UseFolder (case get input "directory" of String path -> T.unpack path; _ -> ""))
  _ -> Nothing

termsVersion :: Text
termsVersion = "2026-11-18"

termsPath :: IO FilePath
termsPath = do
  root <- configDirectory
  containedPath root (root </> "terms.json")

termsAccepted :: IO Bool
termsAccepted = do
  result <- try $ do
    value <- termsPath >>= \path -> readJSON path Null
    pure (get value "accepted"==Bool True && get value "version"==String termsVersion)
  pure (either (const False) id (result :: Either IOException Bool))

acceptTerms :: Value -> IO ()
acceptTerms input = do
  unless (get input "terms_accepted"==Bool True && get input "terms_version"==String termsVersion) $
    storageError "Accept the Terms of Service and Privacy Policy before continuing."
  accepted <- termsAccepted
  unless accepted $ do
    path <- termsPath
    time <- floor <$> getPOSIXTime :: IO Int64
    writeJSON path (object ["accepted" .= True,"version" .= termsVersion,"accepted_at" .= time])

requireTerms :: IO ()
requireTerms = termsAccepted >>= \accepted -> unless accepted $
  storageError "Accept the Terms of Service and Privacy Policy before continuing."

startupTelemetryArguments :: IO [String]
startupTelemetryArguments = do
  requireTerms
  path <- (</> "telemetry.json") <$> configDirectory
  saved <- doesFileExist path
  pure (if saved then [] else [startupConsentOption,"true"])

bootstrapTermsScript :: IO Text
bootstrapTermsScript = do
  accepted <- termsAccepted
  let json value = TE.decodeUtf8 (BL.toStrict (encode value))
  pure ("window.setTermsState?.(" <> json accepted <> "," <> json termsVersion <> ")")

legalDocument :: Text -> Maybe FilePath
legalDocument name = lookup name
  [("terms-of-service","app/shell/terms-of-service.html"),("privacy-policy","app/shell/privacy-policy.html")]

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
  source <- TIO.readFile (runtime </> ".build/web/preferences.js")
  token <- UUID.nextRandom
  let arguments = TE.decodeUtf8 (BL.toStrict (encode (address, directory, saved, token)))
  pure (source <> "\nwindow.sapiensPreferences(" <> arguments <> ");")
