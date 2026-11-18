module Sapiens.BrowserFFI where

import Prelude
import Control.Promise (Promise, toAffE)
import Data.Argonaut.Core (Json)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Web.Event.Event (Event)

foreign import data Socket :: Type
foreign import data Camera :: Type
foreign import data Voice :: Type
foreign import data Response :: Type
foreign import environment :: Effect Json
foreign import now :: Effect Number
foreign import uuid :: Effect String
foreign import request :: String -> String -> String -> Json -> Effect (Promise Json)
foreign import storageRead :: String -> Effect String
foreign import storageWrite :: String -> String -> Effect Boolean
foreign import storageRemove :: String -> Effect Unit
foreign import storageKeys :: Effect (Array String)
foreign import database :: String -> Int -> Array String -> String -> String -> String -> Json -> Effect (Promise Json)
foreign import openSocket :: String -> Effect Unit -> (String -> Effect Unit) -> (Int -> Effect Unit) -> Effect Socket
foreign import socketSend :: Socket -> String -> Effect Unit
foreign import socketClose :: Socket -> Effect Unit
foreign import subscribe :: String -> String -> (Json -> Effect Unit) -> Effect (Effect Unit)
foreign import callback :: String -> (Json -> Effect Unit) -> Effect (Effect Unit)
foreign import eventData :: Event -> Effect Json
foreign import focus :: String -> Effect Unit
foreign import selectRange :: String -> Int -> Int -> Effect Unit
foreign import measure :: String -> Effect Json
foreign import measureChildren :: String -> Effect (Array Json)
foreign import scroll :: String -> Number -> Effect Unit
foreign import scrollIntoView :: String -> Effect Unit
foreign import cssProperty :: String -> String -> Effect String
foreign import setStyle :: String -> String -> String -> Effect Unit
foreign import setAttribute :: String -> String -> String -> Effect Unit
foreign import title :: String -> Effect Unit
foreign import nativeMessage :: Json -> Effect Unit
foreign import showDialog :: String -> Boolean -> Effect Unit
foreign import historyPush :: Json -> Effect Unit
foreign import historyReplace :: String -> Effect Unit
foreign import historyBack :: Effect Unit
foreign import reload :: Effect Unit
foreign import confirm :: String -> Effect Boolean
foreign import clipboard :: String -> Effect (Promise Unit)
type FileSelection =
  { name :: String
  , mime :: Maybe String
  , size :: Number
  , data :: String
  }

foreign import chooseFileImpl :: String -> Int -> Effect (Promise (Nullable
  { name :: String
  , mime :: Nullable String
  , size :: Number
  , data :: String
  }))

chooseFile :: String -> Int -> Aff (Maybe FileSelection)
chooseFile accept limit = do
  selected <- toAffE $ chooseFileImpl accept limit
  pure $ map (\file -> file {mime = toMaybe file.mime}) $ toMaybe selected

foreign import decodeQR :: String -> Int -> Effect (Promise Json)
foreign import cameraStart :: String -> Effect (Promise Camera)
foreign import cameraStop :: Camera -> Effect Unit
foreign import cameraQR :: String -> Int -> Effect Json
foreign import voiceStart :: String -> Int -> (Json -> Effect Unit) -> Effect (Promise Voice)
foreign import voiceStop :: Voice -> Effect (Promise Json)
foreign import qrImage :: String -> Effect (Promise String)
foreign import parseMarkup :: String -> Json
foreign import parseURL :: String -> String -> Json
foreign import encodeURI :: String -> String
foreign import decodeURI :: String -> String
foreign import base64Encode :: String -> String
foreign import base64Decode :: String -> Json
foreign import byteKey :: Json -> String
foreign import keyPair :: Effect Json
foreign import startHostCrypto :: Effect (Promise Unit)
foreign import publicKey :: String -> String
foreign import byteLength :: String -> Int
foreign import randomBytes :: Int -> Effect String
foreign import box :: String -> String -> String -> String -> String
foreign import unbox :: String -> String -> String -> String -> String
foreign import digest :: String -> Array String -> Effect (Promise (Array Int))
foreign import parseTime :: String -> Number
foreign import formatDate :: String -> Json -> String -> String
foreign import registerWorker :: String -> Effect (Promise Unit)
foreign import updateWorker :: Effect (Promise Unit)
foreign import installPrompt :: Effect (Promise Unit)
foreign import workerInstall :: String -> (Json -> Effect (Promise Unit)) -> Effect Unit
foreign import workerFetch :: (Json -> Boolean) -> (Json -> Effect (Promise Response)) -> Effect Unit
foreign import cacheAll :: String -> Array String -> Effect (Promise Unit)
foreign import cacheKeys :: Effect (Promise (Array String))
foreign import cacheDelete :: String -> Effect (Promise Unit)
foreign import cacheResponse :: String -> String -> Effect (Promise Response)
foreign import fetchResponse :: String -> Effect (Promise Response)
foreign import activateWorker :: Effect (Promise Unit)

foreign import requestBytes :: String -> Json -> Effect (Promise Json)
foreign import removeAttribute :: String -> String -> Effect Unit
foreign import timestamp :: Number -> String

foreign import styleSheet :: String -> String -> Effect Unit
foreign import callbackResult :: String -> (Json -> Effect Json) -> Effect Unit

foreign import subscribeEvent :: String -> String -> (Event -> Effect Unit) -> Effect (Effect Unit)

foreign import subscribeMedia :: String -> Effect Unit -> Effect (Effect Unit)
