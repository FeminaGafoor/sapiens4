module UpdateState where

import Control.Monad
import Control.Exception (catchJust)
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

newtype Version = Version { versionName :: String } deriving (Eq,Show)
data Previous = Previous { previousVersion :: Version, previousBackup :: Version, previousDigest :: String } deriving (Eq,Show)
data Selection = Selection { current :: Version, previous :: Maybe Previous } deriving (Eq,Show)
data PendingUpdate = PendingUpdate
 { original :: Selection, target :: Version, attempt :: Version
 , dataRoot :: FilePath, configRoot :: FilePath, dataIdentity :: String, configIdentity :: String, backupDigest :: String
 } deriving (Eq,Show)
data UpdateState = Settled Selection | Pending PendingUpdate deriving (Eq,Show)

instance ToJSON Version where toJSON = toJSON . versionName
instance FromJSON Version where
 parseJSON = withText "version identifier" $ \s -> case UUID.fromText s of
  Just uuid | UUID.toText uuid == s -> pure (Version (T.unpack s))
  _ -> fail "Invalid immutable version identifier"
instance ToJSON Previous where
 toJSON p = object ["version" .= previousVersion p,"backup" .= previousBackup p,"digest" .= previousDigest p]
instance FromJSON Previous where
 parseJSON = withObject "previous version and data" $ \o -> do
  exact ["version","backup","digest"] o
  p <- Previous <$> o .: "version" <*> o .: "backup" <*> o .: "digest"
  unless (validDigest (previousDigest p)) (fail "Invalid Previous backup digest")
  pure p
instance ToJSON Selection where
 toJSON s = object ["current" .= current s,"previous" .= previous s]
instance FromJSON Selection where
 parseJSON = withObject "version selection" $ \o -> do
  exact ["current","previous"] o
  s <- Selection <$> o .: "current" <*> o .: "previous"
  when (Just (current s) == (previousVersion <$> previous s)) (fail "Current and Previous must differ")
  pure s
instance ToJSON UpdateState where
 toJSON (Settled s) = object ["format" .= (1::Int),"state" .= ("Settled"::String),"selection" .= s]
 toJSON (Pending p) = object
  ["format" .= (1::Int),"state" .= ("Pending"::String),"original" .= original p
  ,"target" .= target p,"attempt" .= attempt p,"data" .= dataRoot p,"config" .= configRoot p
  ,"dataIdentity" .= dataIdentity p,"configIdentity" .= configIdentity p,"backup" .= backupDigest p]
instance FromJSON UpdateState where
 parseJSON = withObject "update state" $ \o -> do
  format <- o .: "format" :: Parser Int
  unless (format == 1) (fail "Unsupported update state format")
  tag <- o .: "state" :: Parser String
  case tag of
   "Settled" -> exact ["format","state","selection"] o >> Settled <$> o .: "selection"
   "Pending" -> do
    exact ["format","state","original","target","attempt","data","config","dataIdentity","configIdentity","backup"] o
    p <- PendingUpdate <$> o .: "original" <*> o .: "target" <*> o .: "attempt" <*> o .: "data" <*> o .: "config"
     <*> o .: "dataIdentity" <*> o .: "configIdentity" <*> o .: "backup"
    unless (target p /= current (original p) && Just (target p) /= (previousVersion <$> previous (original p))
      && all isAbsolute [dataRoot p,configRoot p] && all (not . null) [dataIdentity p,configIdentity p]
      && validDigest (backupDigest p)) (fail "Invalid pending update")
    pure (Pending p)
   _ -> fail "Unknown update decision"

exact :: [Key] -> Object -> Parser ()
exact names o = unless (length names == K.size o && all (`K.member` o) names) (fail "Unexpected or missing update state fields")

statePath :: FilePath -> FilePath
statePath root = root </> "update-state.json"
versionPath :: FilePath -> Version -> FilePath
versionPath root v = root </> "versions" </> versionName v
backupPath :: FilePath -> Version -> FilePath
backupPath root nonce = root </> "backups" </> versionName nonce
readUpdateState :: FilePath -> IO UpdateState
readUpdateState root = do
 bytes <- B.readFile (statePath root)
 either (fail . ("Invalid update-state.json; launch refused: "++)) pure (eitherDecode bytes)

reservationName :: FilePath
reservationName = ".sapiens-update-owner"

configOwnerName :: FilePath
configOwnerName = ".sapiens-config-owner"

readBinding :: FromJSON a => FilePath -> IO (Maybe a)
readBinding path = do
 binding <- catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
  (Just <$> eitherDecodeFileStrict path) (const (pure Nothing))
 traverse (either fail pure) binding

assertDataAvailable :: FilePath -> IO ()
assertDataAvailable = check True
 where
 check ownsLock directory = do
  binding <- readBinding (directory </> reservationName)
  forM_ binding $ \(owner,nonce) -> do
   unless (isAbsolute owner) (fail "Invalid Data Root update owner")
   state <- readUpdateState owner
   case state of
    Pending pending | attempt pending == nonce -> do
     home <- getExecutablePath >>= installationOf
     ticket <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
     unless (home == Just (owner,target pending) && ticket == Just (versionName nonce)) $
      fail "This Data Root has an unfinished Sapiens update. Launch its original installed shortcut to recover."
    _ -> when ownsLock (removeFile (directory </> reservationName))
  let container = takeDirectory directory
  when (map toLower (takeFileName container) == "corporas" && UUID.fromString (takeFileName directory) /= Nothing) $
   check False (takeDirectory container)

installationOf :: FilePath -> IO (Maybe (FilePath,Version))
installationOf executable = do
 path <- canonicalizePath executable
 case (os,reverse (splitDirectories path)) of
  ("darwin","Sapiens4":"MacOS":"Contents":"Sapiens4.app":rest) -> managed rest
  ("darwin","sapiens4":"bin":".build":"runtime":"Resources":"Contents":"Sapiens4.app":rest) -> managed rest
  ("mingw32",name:bin:build:runtime:rest)
   | map (map toLower) [name,bin,build,runtime] == ["sapiens4.exe","bin",".build","runtime"] -> managed rest
  ("mingw32",name:rest) | map toLower name == "sapiens4.exe" -> managed rest
  _ -> pure Nothing
 where
 managed (name:parent:root@(_:_)) | equalFilePath parent "versions" = case fromJSON (toJSON name) of
  Success version -> pure (Just (joinPath (reverse root),version))
  Error e -> fail e
 managed _ = pure Nothing

validDigest :: String -> Bool
validDigest value = length value == 64 && all (`elem` ("0123456789abcdef"::String)) value
