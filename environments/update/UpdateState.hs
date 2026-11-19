module UpdateState
 ( Version, versionName, freshVersion, parseVersion, canonicalUUID
 , UpdateAttemptId, attemptName, freshAttemptId
 , Sha256, parseSha256, sha256
 , Previous(..), Selection(..), UpdateRoots(..), PendingUpdate(..), UpdateState(..)
 , DataReservation(..), newTarget, candidateAuthorized
 , StagingNotJoined(..), CandidateNotJoined(..)
 , statePath, versionPath, backupPath, containsPath, pathPresentWith, readUpdateState
 , reservationName, configWriterLockName, configOwnerName, readBinding, assertDataAvailable, installationOf
 ) where

import Control.Monad
import Control.Exception (Exception(..), SomeException, catchJust)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.KeyMap as K
import Data.ByteArray.Encoding (Base(Base16), convertToBase)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as B
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory hiding (executable)
import System.Environment (getExecutablePath, lookupEnv)
import System.FilePath
import System.IO.Error (isDoesNotExistError)
import System.Info (os)

newtype Version = Version UUID.UUID deriving (Eq, Show)
newtype UpdateAttemptId = UpdateAttemptId UUID.UUID deriving (Eq, Show)
newtype Sha256 = Sha256 String deriving (Eq, Show)

data Previous = Previous { previousVersion :: Version, previousBackup :: UpdateAttemptId, previousDigest :: Sha256 } deriving (Eq,Show)
data Selection = Selection { current :: Version, previous :: Maybe Previous } deriving (Eq,Show)
data UpdateRoots = UpdateRoots { dataRoot :: FilePath, configRoot :: FilePath } deriving (Eq, Show)
data PendingUpdate = PendingUpdate
 { original :: Selection, target :: Version, attempt :: UpdateAttemptId
 , roots :: UpdateRoots, dataIdentity :: String, configIdentity :: String, backupDigest :: Sha256
 } deriving (Eq,Show)
data UpdateState = Settled Selection | Pending PendingUpdate deriving (Eq,Show)

newtype StagingNotJoined = StagingNotJoined SomeException deriving Show
instance Exception StagingNotJoined where
 displayException (StagingNotJoined problem) =
  "The update installer could not be joined. Staging files were retained. " ++ displayException problem

newtype CandidateNotJoined = CandidateNotJoined SomeException deriving Show
instance Exception CandidateNotJoined where
 displayException (CandidateNotJoined problem) =
  "The candidate process tree could not be joined. Recovery is deferred. Close Sapiens and relaunch from its installed shortcut. " ++ displayException problem
data DataReservation = DataReservation FilePath UpdateAttemptId deriving Eq

canonicalUUID :: T.Text -> Maybe UUID.UUID
canonicalUUID name = do
 uuid <- UUID.fromText name
 guard (UUID.toText uuid == name)
 pure uuid

parseVersion :: String -> Either String Version
parseVersion = maybe (Left "Invalid immutable version identifier") (Right . Version) . canonicalUUID . T.pack

versionName :: Version -> String
versionName (Version uuid) = UUID.toString uuid

attemptName :: UpdateAttemptId -> String
attemptName (UpdateAttemptId uuid) = UUID.toString uuid

freshVersion :: IO Version
freshVersion = Version <$> UUID.nextRandom

freshAttemptId :: IO UpdateAttemptId
freshAttemptId = UpdateAttemptId <$> UUID.nextRandom

parseSha256 :: String -> Maybe Sha256
parseSha256 value
 | length value == 64 && all (`elem` ("0123456789abcdef"::String)) value = Just (Sha256 value)
 | otherwise = Nothing

sha256 :: B.ByteString -> Sha256
sha256 = Sha256 . BC.unpack . convertToBase Base16 . SHA256.hashlazy

instance ToJSON Version where toJSON = toJSON . versionName
instance FromJSON Version where
 parseJSON = withText "version identifier" (either fail pure . parseVersion . T.unpack)
instance ToJSON UpdateAttemptId where toJSON = toJSON . attemptName
instance FromJSON UpdateAttemptId where
 parseJSON = withText "update attempt identifier" $
  maybe (fail "Invalid update attempt identifier") (pure . UpdateAttemptId) . canonicalUUID
instance ToJSON Sha256 where toJSON (Sha256 value) = toJSON value
instance ToJSON DataReservation where toJSON (DataReservation root nonce) = toJSON (root,nonce)
instance FromJSON DataReservation where
 parseJSON value = uncurry DataReservation <$> parseJSON value
instance ToJSON Previous where
 toJSON p = object ["version" .= previousVersion p,"backup" .= previousBackup p,"digest" .= previousDigest p]
instance FromJSON Previous where
 parseJSON = withObject "previous version and data" $ \o -> do
  exact ["version","backup","digest"] o
  digest <- o .: "digest" >>= maybe (fail "Invalid Previous backup digest") pure . parseSha256
  Previous <$> o .: "version" <*> o .: "backup" <*> pure digest
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
  ,"target" .= target p,"attempt" .= attempt p,"data" .= dataRoot (roots p),"config" .= configRoot (roots p)
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
    digest <- o .: "backup" >>= maybe (fail "Invalid pending update") pure . parseSha256
    p <- PendingUpdate <$> o .: "original" <*> o .: "target" <*> o .: "attempt"
     <*> (UpdateRoots <$> o .: "data" <*> o .: "config")
     <*> o .: "dataIdentity" <*> o .: "configIdentity" <*> pure digest
    unless (newTarget (original p) (target p)
      && all isAbsolute [dataRoot (roots p),configRoot (roots p)] && all (not . null) [dataIdentity p,configIdentity p]) (fail "Invalid pending update")
    pure (Pending p)
   _ -> fail "Unknown update decision"

newTarget :: Selection -> Version -> Bool
newTarget selection version = version /= current selection && Just version /= (previousVersion <$> previous selection)

candidateAuthorized :: PendingUpdate -> Version -> Maybe String -> Bool
candidateAuthorized pending version ticket = version == target pending && ticket == Just (attemptName (attempt pending))

exact :: [Key] -> Object -> Parser ()
exact names o = unless (length names == K.size o && all (`K.member` o) names) (fail "Unexpected or missing update state fields")

statePath :: FilePath -> FilePath
statePath root = root </> "update-state.json"
versionPath :: FilePath -> Version -> FilePath
versionPath root v = root </> "versions" </> versionName v
backupPath :: FilePath -> UpdateAttemptId -> FilePath
backupPath root nonce = root </> "backups" </> attemptName nonce

containsPath :: FilePath -> FilePath -> Bool
containsPath parent path = p == q || addTrailingPathSeparator p `isPrefixOf` q
 where
 p = map toLower (normalise parent)
 q = map toLower (normalise path)

pathPresentWith :: (FilePath -> IO Bool) -> FilePath -> IO Bool
pathPresentWith probe path = do
 let parent = takeDirectory (dropTrailingPathSeparator path)
 present <- if equalFilePath parent path then pure True else pathPresentWith probe parent
 if present then probe path else pure False

readUpdateState :: FilePath -> IO UpdateState
readUpdateState root = do
 bytes <- B.readFile (statePath root)
 either (fail . ("Invalid update-state.json; launch refused: "++)) pure (eitherDecode bytes)

reservationName :: FilePath
reservationName = ".sapiens-update-owner"

configWriterLockName :: FilePath
configWriterLockName = ".sapiens-config-writers.lock"

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
  forM_ binding $ \(DataReservation owner nonce) -> do
   unless (isAbsolute owner) (fail "Invalid Data Root update owner")
   state <- readUpdateState owner
   case state of
    Pending pending | attempt pending == nonce -> do
     home <- getExecutablePath >>= installationOf
     ticket <- lookupEnv "SAPIENS_UPDATE_ATTEMPT"
     unless (maybe False (\(root,version) -> root == owner && candidateAuthorized pending version ticket) home) $
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
 managed (name:parent:root@(_:_)) | equalFilePath parent "versions" =
  either fail (\version -> pure (Just (joinPath (reverse root),version))) (parseVersion name)
 managed _ = pure Nothing
