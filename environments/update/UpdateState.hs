module UpdateState where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as K
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.UUID as UUID
import System.Directory
import System.FilePath

newtype Version = Version { versionName :: String } deriving (Eq,Show)
data Previous = Previous { previousVersion :: Version, previousBackup :: Version, previousDigest :: String } deriving (Eq,Show)
data Selection = Selection { current :: Version, previous :: Maybe Previous } deriving (Eq,Show)
data PendingUpdate = PendingUpdate
 { original :: Selection, target :: Version, attempt :: Version
 , dataRoot :: FilePath, configRoot :: FilePath, backupDigest :: String
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
  ,"target" .= target p,"attempt" .= attempt p,"data" .= dataRoot p,"config" .= configRoot p,"backup" .= backupDigest p]
instance FromJSON UpdateState where
 parseJSON = withObject "update state" $ \o -> do
  format <- o .: "format" :: Parser Int
  unless (format == 1) (fail "Unsupported update state format")
  tag <- o .: "state" :: Parser String
  case tag of
   "Settled" -> exact ["format","state","selection"] o >> Settled <$> o .: "selection"
   "Pending" -> do
    exact ["format","state","original","target","attempt","data","config","backup"] o
    p <- PendingUpdate <$> o .: "original" <*> o .: "target" <*> o .: "attempt" <*> o .: "data" <*> o .: "config" <*> o .: "backup"
    unless (target p /= current (original p) && Just (target p) /= (previousVersion <$> previous (original p))
      && all isAbsolute [dataRoot p,configRoot p] && validDigest (backupDigest p)) (fail "Invalid pending update")
    pure (Pending p)
   _ -> fail "Unknown update decision"

exact :: [Key] -> Object -> Parser ()
exact names o = unless (length names == K.size o && all (`K.member` o) names) (fail "Unexpected or missing update state fields")

statePath :: FilePath -> FilePath
statePath root = root </> "update-state.json"
versionPath :: FilePath -> Version -> FilePath
versionPath root v = root </> "versions" </> versionName v
backupPath :: FilePath -> PendingUpdate -> FilePath
backupPath root p = root </> "backups" </> versionName (attempt p)
readUpdateState :: FilePath -> IO UpdateState
readUpdateState root = do
 bytes <- B.readFile (statePath root)
 either (fail . ("Invalid update-state.json; launch refused: "++)) pure (eitherDecode bytes)

-- A state file is mandatory for installed versions; absence never means reset.
installationOf :: FilePath -> IO (Maybe (FilePath,Version))
installationOf executable = do
 path <- canonicalizePath executable
 let parts = splitDirectories path
 case [ (joinPath (take n parts),parts !! (n+1)) | n <- [0..length parts-3], parts !! n == "versions" ] of
  [] -> pure Nothing
  [(root,name)] -> case fromJSON (toJSON name) of
   Success version -> pure (Just (root,version))
   Error e -> fail e
  _ -> fail "Ambiguous installation path"

validDigest :: String -> Bool
validDigest value = length value == 64 && all (`elem` ("0123456789abcdef"::String)) value
