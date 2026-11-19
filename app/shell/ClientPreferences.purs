module Sapiens.ClientPreferences where

import Prelude
import Data.Argonaut.Core (Json, toObject)
import Data.Array as A
import Data.Foldable (for_, traverse_)
import Data.Maybe (fromMaybe)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Foreign (readString)
import Web.Event.CustomEvent as CustomEvent
import Data.String as S
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception as E
import Foreign.Object as O
import Sapiens.BrowserNative as N
import Sapiens.Data as D

allowed :: String -> Boolean
allowed key = key == "sapi-color-theme" || key == "s4.active" || S.take 13 key == "s4.workspace."

install :: Json -> Effect Unit
install arguments = do
  top <- N.topFrame
  environment <- N.environment
  let arg index = fromMaybe D.null $ A.index (D.arr arguments) index
      address = D.str $ arg 0
      root = D.str $ arg 1
      saved = toObject $ arg 2
      token = D.str $ arg 3
  when (top && D.s "origin" environment == D.s "origin" (N.parseURL "" address)) $ void $ E.try do
    let notify key value = when (allowed key) $ N.nativeMessage $ D.json {clientPreferences: {key, value}}
        preferenceKeys = A.filter allowed <$> N.storageKeys
        publish = preferenceKeys >>= traverse_ (\key -> N.storageRead key >>= notify key)
    previous <- N.storageRead "s4.native.storage"
    when (previous /= "" && previous /= root) do
      keys <- N.storageKeys
      traverse_ N.storageRemove $ A.filter (\key -> allowed key || S.take 12 key == "s4.snapshot.") keys
    void $ N.storageWrite "s4.native.storage" root
    restored <- N.sessionRead "s4.native.restore"
    when (restored /= token) $ for_ saved \values -> do
      keys <- preferenceKeys
      traverse_ N.storageRemove $ A.filter (\key -> not (O.member key values)) keys
      traverse_ (\(Tuple key value) -> when (allowed key) $ void $ N.storageWrite key $ D.str value) $ (O.toUnfoldable values :: Array (Tuple String Json))
      N.sessionWrite "s4.native.restore" token
    N.observeStorage notify
    publish
    void $ N.subscribe "window" "sapiens-preferences-ready" $ const publish
    void $ N.subscribeEvent "window" "sapiens-preferences-flush" \event -> do
      N.setAttribute "html" "inert" ""
      keys <- preferenceKeys
      values <- O.fromFoldable <$> traverse (\key -> Tuple key <$> N.storageRead key) keys
      N.nativeMessage $ D.json {preferencesFlush: {nonce: fromMaybe "" (CustomEvent.fromEvent event >>= hush <<< runExcept <<< readString <<< CustomEvent.detail), values}}
    void $ N.subscribeEvent "window" "sapiens-preferences-resume" $ const $ N.removeAttribute "html" "inert"
    void $ N.subscribe "window" "pagehide" $ const publish

main :: Effect Unit
main = void $ N.callback "sapiensPreferences" install
