module Sapiens.Update where

import Prelude
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Sapiens.Data as D
import Sapiens.Model as M
import Sapiens.Navigation as N

-- Events for the migrated navigation and file-preview state.
data Action
  = Select String
  | SetScope N.Scope
  | Navigate N.Activity
  | SetPeriod N.Period
  | Search String
  | ToggleSearch
  | TogglePane N.Pane
  | RequestPreview
  | ReceivePreview M.Request (Either M.PreviewError String)

data Command
  = SaveView
  | SavePreferences
  | LoadDocument
  | ScrollToStart
  | ScrollToBottom
  | FocusSearch
  | OpenWorkspace
  | LoadPreview
  | FetchPreview M.Request

data Transition = Unchanged | Changed M.Model (Array Command) | Commands (Array Command)

update :: Action -> M.Model -> Transition
update action m = case action of
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
