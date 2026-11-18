module Sapiens.Setup where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Traversable (traverse)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.String as S
import Effect (Effect)
import Effect.Ref as Ref
import Sapiens.BrowserNative as N
import Sapiens.Data as D
import Web.Event.Event as Event

data Choice = Fresh | Existing
derive instance eqChoice :: Eq Choice

type Setup = {choice :: Choice, directory :: String, busy :: Boolean, termsVersion :: String}

set :: String -> String -> Json -> Effect Unit
set selector key value = N.queryAll selector >>= traverse_ (\node -> N.setProperty node key value)

accepted :: Effect Boolean
accepted = do
  nodes <- N.queryAll "#terms-consent"
  flags <- traverse (\node -> D.flag <$> N.property node "checked") nodes
  pure $ fromMaybe false $ A.head flags

blocked :: Setup -> Effect Boolean
blocked state = do
  consent <- accepted
  pure $ state.busy || not consent || state.termsVersion == ""

render :: Setup -> Effect Unit
render state = do
  choices <- N.queryAll ".setup-choice"
  traverse_ (\{index, value} -> N.nodeAttribute value "aria-pressed" $ if (index == 0) == (state.choice == Fresh) then "true" else "false") $ A.mapWithIndex (\index value -> {index, value}) choices
  disabled <- blocked state
  set "button" "disabled" $ D.bool disabled
  set "#terms-consent" "disabled" $ D.bool state.busy
  set "#get-started" "disabled" $ D.bool $ disabled || (state.choice == Existing && state.directory == "")
  let selected = state.choice == Existing && state.directory /= ""
  N.setAttribute "#storage-existing" "data-change" $ if selected then "true" else "false"
  set "#storage-change" "hidden" $ D.bool $ not selected
  let parts = A.filter (_ /= "") $ S.split (S.Pattern "/") $ S.replaceAll (S.Pattern "\\") (S.Replacement "/") state.directory
      path = (if A.length parts > 2 then "…/" else "") <> S.joinWith "/" (A.takeEnd 2 parts)
  set "#storage-directory" "textContent" $ D.text $ if state.directory == "" then "Open existing data or start in an empty folder." else path
  set "#storage-directory" "title" $ D.text state.directory

main :: Effect Unit
main = do
  state <- Ref.new {choice: Fresh, directory: "", busy: false, termsVersion: ""}
  let
    update = Ref.read state >>= render
    showSetup = update *> set "main" "hidden" (D.bool false)
    post message = do
      current <- Ref.read state
      consent <- accepted
      N.nativeMessage $ D.merge message $ D.json {terms_accepted: consent, terms_version: current.termsVersion}
    loading = Ref.modify_ (_ {busy = true}) state *> update
    pick = do
      current <- Ref.read state
      disabled <- blocked current
      unless disabled do
        loading
        post $ D.json {storage: "pick"}
    select choice = do
      current <- Ref.read state
      disabled <- blocked current
      unless disabled $ if choice == Existing && current.directory == "" then pick else Ref.modify_ (_ {choice = choice}) state *> update
    legal document event = do
      Event.stopPropagation event
      native <- N.nativeAvailable
      when native do
        Event.preventDefault event
        post $ D.json {storage: "document", document}
  N.callback2 "setTermsState" \consent version -> do
    set "#terms-consent" "checked" consent
    Ref.modify_ (_ {termsVersion = D.str version}) state
    update
  void $ N.callback "storagePicked" \directory -> do
    Ref.modify_ (\s -> if D.str directory == "" then s {busy = false} else s {busy = false, directory = D.str directory, choice = Existing}) state
    update
  void $ N.callback "showStorageSetup" $ const showSetup
  void $ N.callback "storageError" \message -> do
    set "#error" "textContent" message
    Ref.modify_ (_ {busy = false}) state
    showSetup
  void $ N.callback "storageLoading" $ const loading
  N.listen "#storage-new" "click" $ const $ select Fresh
  N.listen "#storage-pick" "click" $ const $ select Existing
  N.listen "#storage-change" "click" $ const pick
  N.listen "#terms-consent" "change" $ const update
  N.listen "#terms-link" "click" $ legal "terms-of-service"
  N.listen "#privacy-link" "click" $ legal "privacy-policy"
  N.listen "#get-started" "click" \event -> when (N.detail event <= 1) do
    current <- Ref.read state
    disabled <- blocked current
    unless (disabled || (current.choice == Existing && current.directory == "")) do
      loading
      post $ D.json {storage: if current.choice == Fresh then "new" else "existing", directory: current.directory}
