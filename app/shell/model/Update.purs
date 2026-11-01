module Sapiens.Update where

import Prelude
import Data.Either (Either(..), either)
import Data.Array as A
import Data.Argonaut.Core (Json)
import Data.Set as Set
import Data.String as S
import Foreign.Object as O
import Data.Maybe (Maybe(..), fromMaybe)
import Sapiens.Data as D
import Sapiens.Model as M
import Sapiens.Navigation as N

-- Events for the migrated navigation and file-preview state.
data Action
  = RequestDocument
  | ReceiveDocument M.Request (Either String Json)
  | RetryDocument String
  | Draft String
  | Send
  | MessageReceived Submission (Either String Unit)
  | RemoveAttachment String
  | ToggleExpanded String
  | Select String
  | SetScope N.Scope
  | Navigate N.Activity
  | SetPeriod N.Period
  | Search String
  | ToggleSearch
  | TogglePane N.Pane
  | RequestPreview
  | ReceivePreview M.Request (Either M.PreviewError String)

data Command
  = FetchDocument M.Request
  | LoadMemoImages Json
  | SendMessage Submission
  | Notify String
  | PollNow
  | ResizeComposer
  | UpdateMention
  | SaveView
  | SavePreferences
  | LoadDocument
  | ScrollToStart
  | ScrollToBottom
  | FocusSearch
  | OpenWorkspace
  | LoadPreview
  | FetchPreview M.Request

type Submission =
  { epoch :: Int, owner :: String, text :: String, attachments :: Array String
  , workload :: Maybe String, path :: String }

submission :: M.Model -> Maybe Submission
submission m =
  let text = S.trim $ M.draft m
      items = M.attachments m
      owner = M.selected m
  in if (text == "" && A.null items) || Set.member m.selected m.sending || m.uploading > 0 || not m.online || m.busy || D.b "retired" owner || D.b "archived" owner || A.elem (D.str $ D.at ["corpora","status"] m.snapshot) ["paused","archived"] then Nothing
     else Just {epoch:m.epoch,owner:m.selected,text,attachments:map (D.s "id") items,path:M.ownerPath m <> "/messages",workload:case m.clarification of
       Just work | D.s "owner" work == m.selected -> Just $ D.s "id" work
       _ -> Nothing}

data Transition = Unchanged | Changed M.Model (Array Command) | Commands (Array Command)

update :: Action -> M.Model -> Transition
update action m = case action of
  RequestDocument -> case M.currentDocument m of
    Nothing -> Unchanged
    Just request ->
      let old = fromMaybe M.NotAsked $ O.lookup request.key m.documents
      in if (_.revision <$> M.requestOf old) == Just request.revision then Unchanged
         else Changed (m {sequence=request.sequence,documents=O.insert request.key (M.Loading request $ M.valueOf old) m.documents}) [FetchDocument request]
  ReceiveDocument request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise -> case O.lookup request.key m.documents >>= M.requestOf of
        Just pending | pending == request -> Changed
          (m {documents=O.insert request.key (either (M.Failed request) (M.Ready request) result) m.documents})
          (case result of
            Right document | S.take 5 request.key == "memo:" -> [LoadMemoImages document]
            _ -> [])
        _ -> Unchanged
  RetryDocument key -> Changed (m {documents=O.delete key m.documents}) [LoadDocument]
  Draft value -> Changed (m {drafts=O.insert m.selected value m.drafts}) [ResizeComposer,UpdateMention,SavePreferences]
  Send -> case submission m of
    Nothing -> Unchanged
    Just request -> Changed (m {sending=Set.insert m.selected m.sending,mention=Nothing}) [SendMessage request]
  MessageReceived request result
    | request.epoch /= m.epoch -> Unchanged
    | otherwise ->
        let current = m {sending=Set.delete request.owner m.sending}
        in case result of
          Left failure -> Changed current [Notify failure]
          Right _ -> Changed
            (current {drafts=if S.trim (fromMaybe "" $ O.lookup request.owner m.drafts) == request.text then O.insert request.owner "" m.drafts else m.drafts
              , attachments=O.insert request.owner (A.filter (\item -> not $ A.elem (D.s "id" item) request.attachments) $ fromMaybe [] $ O.lookup request.owner m.attachments) m.attachments
              , clarification=Nothing})
            [SavePreferences,ScrollToBottom,PollNow]
  RemoveAttachment id -> Changed (m {attachments=O.alter (Just <<< A.filter ((_ /= id) <<< D.s "id") <<< fromMaybe []) m.selected m.attachments}) [SavePreferences]
  ToggleExpanded key -> Changed (m {expanded=(if Set.member key m.expanded then Set.delete else Set.insert) key m.expanded}) []
  Select id -> Changed (M.select id m) [ScrollToBottom,LoadPreview,SaveView]
  SetScope scope -> Changed
    (m {scope=scope,groupView=Nothing,panes=if N.limit m.env < 3 then N.togglePane false (N.limit m.env) (D.valid $ M.activeTab m) N.Sidebar m.panes else m.panes})
    [SaveView]
  Navigate activity -> Changed (m {activity=activity,menu=Nothing}) [ScrollToStart,LoadDocument,SaveView]
  SetPeriod period -> Changed
    (m {menu=Nothing,activity=case m.activity of
      N.Work view _ -> N.Work view period
      other -> other})
    [SavePreferences]
  Search search -> Changed (m {search=search}) []
  ToggleSearch -> Changed
    (m {searchOpen=not m.searchOpen,search=if m.searchOpen then "" else m.search})
    (if m.searchOpen then [] else [FocusSearch])
  TogglePane pane
    | pane == N.Workspace && not (D.valid $ M.activeTab m) -> Commands [OpenWorkspace]
    | otherwise -> Changed (m {panes=N.togglePane true (N.limit m.env) (D.valid $ M.activeTab m) pane m.panes}) [SaveView]
  RequestPreview -> case m.preview of
    M.Loading _ _ -> Unchanged
    _ -> case M.currentPreview m of
      Nothing -> Unchanged
      Just request ->
        let previous = if (_.path <$> M.requestOf m.preview) == Just request.path then M.valueOf m.preview else Nothing
        in Changed (m {sequence=request.sequence,preview=M.Loading request previous}) [FetchPreview request]
  ReceivePreview requested result -> case m.preview of
    M.Loading pending _
      | requested.epoch == m.epoch && requested == pending ->
          Changed (m {preview=either (M.Failed requested) (M.Ready requested) result}) []
    _ -> Unchanged
