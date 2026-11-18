module Sapiens.BrowserDatabase where

import Prelude
import Control.Alt ((<|>))
import Control.Promise (Promise, fromAff)
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), attempt, bracket, delay, effectCanceler, makeAff, parallel, sequential, throwError)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Sapiens.Data as D

foreign import data Database :: Type
foreign import openDatabase :: String -> Int -> Array String -> (String -> Effect Unit) -> (Database -> Effect Unit) -> Effect (Effect Unit)
foreign import closeDatabase :: Database -> Effect Unit
foreign import transact :: Database -> String -> String -> String -> Array Json -> (String -> Effect Unit) -> (Json -> Effect Unit) -> Effect (Effect Unit)

database :: String -> Int -> Array String -> String -> String -> String -> Json -> Effect (Promise Json)
database name version stores operation store key value = fromAff $ race
  (delay (Milliseconds 3000.0) *> throwError (error "Device storage is unavailable."))
  (do
    unless (A.elem operation ["get", "getAll", "put", "delete"]) $ throwError $ error "Unsupported storage operation"
    let mode = if A.elem operation ["get", "getAll"] then "readonly" else "readwrite"
        args = case operation of
          "getAll" -> []
          "put" -> [value, D.text key]
          _ -> [D.text key]
        failure message = error $ case message of
          "blocked" -> "Close other tabs and try again."
          "abort" -> "Device storage failed."
          _ -> message
    bracket
      (makeAff \done -> effectCanceler <$> openDatabase name version stores (done <<< Left <<< failure) (done <<< Right))
      (liftEffect <<< closeDatabase)
      (\connection -> makeAff \done -> effectCanceler <$> transact connection mode store operation args (done <<< Left <<< failure) (done <<< Right)))

race :: forall a. Aff a -> Aff a -> Aff a
race left right = do
  result <- sequential $ parallel (attempt left) <|> parallel (attempt right)
  case result of
    Left failure -> throwError failure
    Right value -> pure value
