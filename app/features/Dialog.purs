module Sapiens.Dialog where

import Sapiens.Form as F
import Prelude
import Data.Argonaut.Core (Json)
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S
import Data.Tuple (Tuple(..))
import Foreign.Object as O
import Halogen.HTML as C
import Libs4.Browser as B
import Sapiens.Action as Intent
import Sapiens.Action (Action(..), Success(..))
import Sapiens.Data as D
import Sapiens.Domain as X
import Sapiens.Html as V
import Sapiens.Model as M
import Sapiens.Navigation as N
import Sapiens.Remote as Remote
import Sapiens.Text as T
import Sapiens.Work as W

nameHelp :: String
nameHelp = "Start with A–Z. Letters, numbers, and - _ . : # + | ( ) & $ ^ are allowed. No spaces."

make :: M.DialogKind -> F.Context -> M.Model -> M.Dialog
make kind record m = {kind,title,record,fields,content,status:M.Editing,identity:m.sequence+1}
  where
  content = if kind == M.Models then F.ModelPreview $ F.modelDraft m.snapshot.main_agent_id else F.NoContent
  name = F.contextName record
  title = case kind of
    M.AppMenu -> D.fallback "CORPORA" $ m.snapshot.corpora.name
    M.WorkspaceSettings -> "Workspace & settings"
    M.Models -> "Models"
    M.Computer -> "Shared computer"
    M.AgentSettings -> name <> " settings"
    M.GroupSettings -> name <> " settings"
    M.Attach -> "Attach"
    M.Reference X.LinkAttachment -> "Add link"
    M.Reference _ -> "Add filepath"
    M.DecisionPrompts -> "Decision prompts"
    M.GroupTask -> if F.contextId record == "" then "Create Group task" else "Edit Group task"
    M.CreateCorpora -> "Create local corpora"
    M.EditCorpora -> "Message to ChiefSapi"
    M.ConnectRemote -> "Connect remote corpora"
    M.HostPairing -> if (F.contextCorpora record).paired then "Edit Pairing" else "New Pairing"
    M.RelayList -> "Remote Control Relay Servers"
    M.RelayAdd -> if F.contextId record == "" then "Add your own relay" else "Edit relay"
    M.RelaySwitch -> if (F.contextChange record).saving then "Change connection?" else "Switch server?"
    M.RelayTerms -> "Use beta relay?"
    M.Installation -> "Add to Home Screen"
    M.Notice -> "Connection notice"
  fields = case kind of
    M.AgentSettings -> base {mode=(orchestration m $ F.contextId record).mode,manager=(orchestration m $ F.contextId record).manager}
    M.GroupTask -> case record of
      F.TaskRecord _ -> base
      _ -> base {assignee=(M.selected m).lead,state="backlog"}
    M.EditCorpora -> base {message=replaceFields (D.fallback m.snapshot.corpora.settings_template (F.contextCorpora record).settings_template) [Tuple "name" (F.contextCorpora record).name,Tuple "device" (F.contextCorpora record).device,Tuple "scope" (F.contextCorpora record).scope]}
    _ -> base
  base = F.fieldsFor record
  replaceFields template replacements = case A.uncons replacements of
    Nothing -> template
    Just {head:Tuple key value,tail} -> replaceFields (S.replaceAll (S.Pattern $ "{" <> key <> "}") (S.Replacement value) template) tail

view :: M.Model -> V.Html
view m = case m.dialog of
  Nothing -> V.node "dialog" [V.id "modal"] []
  Just d -> V.node "dialog" [V.id $ if M.corporaDialog d.kind then "corpora-dialog" else "modal",V.cls $ if d.kind == M.AppMenu then "app-menu-sheet" else if d.kind == M.Models then "model-settings-dialog" else "",V.attr "aria-labelledby" "dialog-title",V.on "cancel" \e -> Link e CloseDialog]
    [V.div (if M.corporaDialog d.kind then "corpora-menu-heading" else "modal-top") [V.span [V.cls "tiny-label"] "SAPIENS4",V.icon (if M.corporaDialog d.kind then "Close" else "Close dialog") "×" CloseDialog]
    ,V.node "div" [V.id $ if M.corporaDialog d.kind then "corpora-dialog-body" else "modal-content"] [V.textNode "h2" [V.id "dialog-title"] (d.title),body m d,V.textNode "p" [V.cls "corpora-feedback",V.attr "role" "status"] ((M.dialogError d))]
    ,V.when (d.kind == M.Models) $ V.div "model-save-row"
      [V.control [V.cls "button primary",V.disabled $ M.dialogBusy d || case d.content of
        F.ModelPreview draft -> not (F.modelDraftValid draft && F.modelDraftChanged draft)
        _ -> true] "Save and apply" (DialogCommand Intent.SaveModelSettings)]]

body :: M.Model -> M.Dialog -> V.Html
body m d = case d.kind of
  M.AppMenu -> V.node "nav" [V.label "Corpora menu"] [V.button "app-menu-row" "Workspace & settings" $ OpenDialog M.WorkspaceSettings F.NoRecord,V.button "app-menu-row" "Shared computer" $ OpenDialog M.Computer F.NoRecord]
  M.WorkspaceSettings -> V.div "workspace-settings"
    [V.node "nav" [V.label "Workspace settings"]
      [settingsLink "Models" "Provider, model and per-Sapi choices." M.Models
      ,settingsLink "Decision prompts" "Instructions used at each decision step." M.DecisionPrompts]
    ,V.node "details" [V.cls "workspace-settings-help"]
      [V.textNode "summary" [] "About this workspace"
      ,V.p "" "Talk to your Sapis in chat, manage their notes, and save documents in their workspaces. Sapis can delegate requests through ChiefSapi."
      ,V.p "" "Use the corpora menu to switch workspaces. Active and Paused describe execution on the host; Connecting means this device is restoring its connection."]]
  M.Models -> case d.content of
    F.ModelPreview draft | draft.loaded -> V.node "fieldset" [V.cls "model-settings-controls",V.disabled $ M.dialogBusy d] [modelSettings m draft]
    F.ModelPreview _ -> if M.dialogBusy d then V.p "model-hint" "Loading saved settings…" else V.button "button" "Retry loading settings" $ DialogCommand Intent.ReloadModelSettings
    _ -> V.p "" "Close and reopen Models to load settings."
  M.Computer -> V.div "" [V.p "" "Blindly4 is the main computer-use tool. Ask a Sapi in chat to work on your computer.",V.div "settings-row" [V.span [] (if m.snapshot.computer.built then "Blindly4 is built" else "Build required: run " <> if windows then ".\\start.ps1" else "./start.sh"),V.span [V.cls "tag"] (if owner == "" then "Available" else "In use · " <> (M.agent m owner).name)],V.p "" $ "Sapis share one computer. " <> if windows then "Windows UI Automation works with accessible apps on your desktop. Elevated apps and secure system dialogs cannot be controlled." else "macOS Accessibility access is required for desktop interaction; permission failures appear in chat."]
  M.AgentSettings -> form "Save"
    [V.node "div" [V.cls "settings-tabs",V.attr "role" "tablist",V.label "Sapi settings"] $ map (\caption -> V.textNode "button" [V.attr "type" "button",V.attr "role" "tab",V.attr "aria-selected" $ show $ caption == "Profile",V.disabled $ caption /= "Profile"] (caption)) ["Profile","Context","Usage","Limits"]
    ,input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "24",V.attr "aria-describedby" "name-help",V.attr "autocomplete" "off"]
    ,V.node "div" [V.cls "name-suggestions",V.id "name-help",V.label "Suggested names"] $ map (\name -> V.button "button" name $ Field F.Name name) suggestions <> [V.icon "More name suggestions" "↻" $ DialogCommand Intent.SuggestNames]
    ,input "Role" F.Role [V.bool "required" true,V.attr "maxlength" "60"]
    ,if F.contextId d.record == m.snapshot.main_agent_id then V.div "settings-row" [V.text "Main orchestrator"] else select "Manager" F.Manager $ map (\a -> Tuple (a.id) (a.name <> if a.id == m.snapshot.main_agent_id then " · Main orchestrator" else "")) managers
    ,select "Work mode" F.Mode [Tuple "normal" "Normal · high",Tuple "deep" "Deep work · xhigh"]
    ,V.p "form-hint" $ "Each call has a " <> show (execution.timeout / 60.0) <> "-minute limit in either mode."]
  M.GroupSettings -> form "Save Group"
    [input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "60"],textarea "Purpose" F.Description 2 [V.attr "maxlength" "2000"]
    ,select "Lead" F.Lead $ map (\a -> Tuple (a.id) $ a.name) sapis
    ,V.node "fieldset" [V.cls "group-members"] $ [V.textNode "legend" [] "Members · at least two, including the Lead"] <> map (\a -> V.node "label" [] [V.node "input" [V.attr "type" "checkbox",V.attr "name" "member",V.value $ a.id,V.bool "checked" $ A.elem a.id d.fields.members,V.check $ CheckField $ a.id] [],T.avatar a "mini" false,V.span [] (a.name)]) sapis
    ,V.p "form-hint" "Each Sapi can belong to up to 11 active Groups."
    ,V.button "button" (if F.contextArchived d.record then "Restore Group" else "Archive Group") $ Mutate ("/api/groups/" <> F.contextId d.record) "PUT" (D.json {revision: D.json $ F.contextRevision d.record, archived: not $ F.contextArchived d.record}) (CloseAndRefresh "Group saved.")]
  M.Attach -> V.div "attachment-menu" $ map (\(Tuple key caption) -> V.button "button" caption $ AttachmentKind key) [Tuple X.ImageAttachment "Image",Tuple X.DocumentAttachment "Document",Tuple X.LinkAttachment "Link",Tuple X.FileAttachment "Filepath"]
  M.Reference kind -> form "Attach" [input (if kind == X.LinkAttachment then "Link" else "Local filepath") F.Value [V.attr "type" $ if kind == X.LinkAttachment then "url" else "text",V.attr "placeholder" $ if kind == X.LinkAttachment then "https://…" else "/Users/…/file.pdf",V.bool "required" true,V.attr "maxlength" "4096"]]
  M.DecisionPrompts -> if d.content == F.NoContent then V.p "" "Loading…" else form "Save prompt" [select "Node" F.Node $ map (\p -> Tuple p.node p.node) $ F.prompts d.content,V.textNode "small" [V.cls "prompt-path"] (field F.Path),textarea "Prompt" F.Content 16 [V.bool "required" true,V.attr "maxlength" "20000"],V.p "" "Changes apply to subsequent decisions. Saved decision records keep their original prompt."]
  M.GroupTask -> form (if F.contextDeleted d.record then "Restore task" else "Save task")
    [input "Title" F.Title [V.bool "required" true,V.attr "maxlength" "500"],textarea "Work to do" F.Body 5 [V.attr "maxlength" "16000"]
    ,select "Assignee" F.Assignee $ map (\id -> Tuple id $ (M.agent m id).name) $ D.strings $ D.json (M.selected m).members
    ,V.when (F.contextId d.record /= "") $ select "Status" F.State $ map (\state -> Tuple state $ W.taskState $ X.groupState state) ["backlog","in_progress","done"]
    ,V.when (F.contextId d.record /= "" && not (F.contextDeleted d.record)) $ V.button "button" "Delete task" $ DialogCommand Intent.DeleteTask
    ,V.p "form-hint" "Saving keeps this as shared work. Use Run task to call the assignee. Deletions can be restored."]
  M.CreateCorpora -> form "Create" [input "Corpora name" F.Name [V.bool "required" true,V.attr "maxlength" "200"],textarea "Scope of work" F.Scope 4 [V.attr "maxlength" "16000"]]
  M.EditCorpora -> form "Send to ChiefSapi ↑" [textarea "Message to ChiefSapi" F.Message 8 [V.bool "required" true]]
  M.ConnectRemote -> V.div ""
    [V.p "" "On the host, open the corpora menu and choose New Pairing in its Remote column."
    ,V.node "section" [V.id "pair-panel",V.hidden $ field F.Code /= ""]
      [V.div "" [V.control [V.id "scan-qr",V.disabled $ (M.dialogBusy d) || d.fields.scanning] "Scan QR" (ScanQR),V.button "qr-upload" "Upload QR" UploadQR]
      ,V.node "div" [V.id "scanner",V.hidden $ not $ d.fields.scanning] [V.node "video" [V.id "qr-camera",V.bool "autoplay" true,V.bool "playsInline" true,V.bool "muted" true] [],V.button "" "Cancel scan" StopScan]
      ,V.node "form" [V.id "paste-invitation",V.submit Pair] [textarea "Or paste an invitation" F.Invitation 2 [V.bool "required" true,V.bool "spellcheck" false,V.attr "autocomplete" "off"],V.textNode "button" [V.attr "type" "submit",V.disabled (M.dialogBusy d)] "Connect"]]
    ,V.node "section" [V.id "verify-panel",V.hidden $ field F.Code == ""] [V.p "" "Approve this matching code on the host.",V.textNode "strong" [V.id "confirmation-code"] (field F.Code)]]
  M.HostPairing -> hostPairing m d
  M.RelayList -> relayList d
  M.RelayAdd -> form "Save" [V.p "" "",V.link "https://github.com/aleski-green/sapiens4/blob/codex/remote-control-mvp/documentation/specs/RemoteMVP.md" "Deploy on GitHub ↗",input "Name" F.Name [V.bool "required" true,V.attr "maxlength" "200",V.attr "autocomplete" "off"],textarea "Connection JSON" F.Connection 6 [V.bool "required" true,V.bool "spellcheck" false,V.attr "maxlength" "4096",V.attr "placeholder" "Paste your relay.connection.json"],V.button "qr-upload" "Import JSON" $ DialogCommand Intent.ImportRelay,V.p "relay-private" "Stored privately on this desktop.",V.button "" "Cancel" $ DialogCommand Intent.BackRelay]
  M.RelaySwitch -> V.div "" [V.node "dl" [V.cls "pairing-details"] [V.textNode "dt" [] "From",V.textNode "dd" [] ((F.contextChange d.record).from),V.textNode "dt" [] "To",V.textNode "dd" [] ((F.contextChange d.record).to)],V.p "" "Switching revokes all paired devices and remote corpora connections for this local app.",V.p "" "Pair all devices and reconnect all remote corpora again.",V.div "pairing-actions" [V.button "" "Cancel" $ DialogCommand Intent.BackRelay,V.control [V.disabled (M.dialogBusy d)] "Switch & revoke all" (DialogCommand Intent.ConfirmRelay)]]
  M.RelayTerms -> V.div "" [V.p "" "For sensitive work, use your own securely maintained relay.",V.p "" "Provided “as is,” without warranties. Use at your own risk.",V.p "" "To the extent permitted by law, authors, contributors, and operators disclaim liability for losses or damages from this service.",V.p "" "Non-excludable rights and liabilities remain unaffected.",V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" $ field F.Accepted == "true",V.check $ \b -> Field F.Accepted $ show b] [],V.text "I accept these beta terms."],V.div "pairing-actions" [V.button "" "Back" $ DialogCommand Intent.BackRelay,V.button "" "Use my relay" $ DialogCommand Intent.AddRelay,V.control [V.disabled $ field F.Accepted /= "true" || (M.dialogBusy d)] "Use beta" (DialogCommand Intent.AcceptRelay)]]
  M.Installation -> V.div "" [V.node "ol" [] $ map (\s -> V.textNode "li" [] (s)) (if ios then ["In Safari, tap Share.","Add to Home Screen. Open as Web App on → Add.","Open Sapiens4 from Home Screen."] else ["In Chrome, tap ⋮.","Install and create shortcut. Then tap Install.","Open Sapiens4 from Home Screen."]),V.button "" "Install Sapiens4" $ DialogCommand Intent.InstallPrompt,V.link "https://sapiens4.ai/help?q=add2homescreen" "Need help? ↗"]
  M.Notice -> V.div "" [V.p "" $ field F.Message,V.button "" "OK" CloseDialog]
  where
  field key = F.fieldValue key d.fields
  settingsLink caption description kind = V.controlWith [V.cls "workspace-settings-link",V.label caption]
    [V.node "span" [] [V.textNode "strong" [] caption,V.textNode "small" [] description],V.chevron true] (OpenDialog kind F.NoRecord)
  input caption key props = V.field "input" caption key (field key) props []
  textarea caption key rows props = V.field "textarea" caption key (field key) ([V.attr "rows" $ show rows] <> props) []
  select caption key options = V.field "select" caption key (field key) [] $ map (\(Tuple value caption_) -> V.textNode "option" [V.value value,V.bool "selected" $ value == field key] caption_) options
  form caption children = V.node "form" [V.cls "form-stack",V.submit SubmitDialog] $ children <> [V.textNode "button" [V.attr "type" "submit",V.cls "button primary",V.disabled (M.dialogBusy d)] (caption)]
  owner = m.snapshot.computer.owner
  windows = m.snapshot.computer.platform == "win32"
  execution = orchestration m $ F.contextId d.record
  sapis = A.filter (\a -> not (M.isGroup a || a.retired)) m.agents
  managers = A.filter (\a -> allowed [] $ a.id) sapis
  allowed seen id = id == "" || id /= F.contextId d.record && not (A.elem id seen) && allowed (A.cons id seen) ((orchestration m id).manager)
  pools = [["Armin","Howl","Seneca","Socrates","Aesop","Nikola","Walt","Akira","Milo"],["Kiki","Chihiro","Sophie","Alice","Hypatia","Simone","Ada","Oprah","Arianna"],["Sage","River","Rowan","Sky","Nova","Phoenix","Ash","Robin","Aster"]]
  suggestions = map (\pool -> let available = A.filter (\name -> not $ A.any (( _ == S.toLower name) <<< S.toLower <<< _.name) m.agents) pool in fromMaybe "Sage" $ A.index (if A.null available then map (_ <> "-" <> show d.identity) pool else available) (mod d.identity $ max 1 $ A.length available)) pools
  ios = N.ios m.env

-- Brand paths from Lobe Icons 1.102.0; see license/PROVIDER_ICONS.md.
providerIcon :: F.ModelProvider -> V.Html
providerIcon provider = svg "svg"
  [V.cls $ "model-provider-icon" <> if provider == F.Claude then " model-provider-icon-claude" else ""
  ,V.attr "viewBox" "0 0 24 24",V.attr "width" "24",V.attr "height" "24"
  ,V.attr "fill" "currentColor",V.attr "fill-rule" "evenodd",V.attr "aria-hidden" "true",V.attr "focusable" "false"]
  [svg "path" [V.attr "d" path,V.attr "clip-rule" "evenodd"] []]
  where
  svg tag = C.elementNS (C.Namespace "http://www.w3.org/2000/svg") (C.ElemName tag)
  path = case provider of
    F.Codex -> "M8.086.457a6.105 6.105 0 013.046-.415c1.333.153 2.521.72 3.564 1.7a.117.117 0 00.107.029c1.408-.346 2.762-.224 4.061.366l.063.03.154.076c1.357.703 2.33 1.77 2.918 3.198.278.679.418 1.388.421 2.126a5.655 5.655 0 01-.18 1.631.167.167 0 00.04.155 5.982 5.982 0 011.578 2.891c.385 1.901-.01 3.615-1.183 5.14l-.182.22a6.063 6.063 0 01-2.934 1.851.162.162 0 00-.108.102c-.255.736-.511 1.364-.987 1.992-1.199 1.582-2.962 2.462-4.948 2.451-1.583-.008-2.986-.587-4.21-1.736a.145.145 0 00-.14-.032c-.518.167-1.04.191-1.604.185a5.924 5.924 0 01-2.595-.622 6.058 6.058 0 01-2.146-1.781c-.203-.269-.404-.522-.551-.821a7.74 7.74 0 01-.495-1.283 6.11 6.11 0 01-.017-3.064.166.166 0 00.008-.074.115.115 0 00-.037-.064 5.958 5.958 0 01-1.38-2.202 5.196 5.196 0 01-.333-1.589 6.915 6.915 0 01.188-2.132c.45-1.484 1.309-2.648 2.577-3.493.282-.188.55-.334.802-.438.286-.12.573-.22.861-.304a.129.129 0 00.087-.087A6.016 6.016 0 015.635 2.31C6.315 1.464 7.132.846 8.086.457zm-.804 7.85a.848.848 0 00-1.473.842l1.694 2.965-1.688 2.848a.849.849 0 001.46.864l1.94-3.272a.849.849 0 00.007-.854l-1.94-3.393zm5.446 6.24a.849.849 0 000 1.695h4.848a.849.849 0 000-1.696h-4.848z"
    F.Claude -> "M4.709 15.955l4.72-2.647.08-.23-.08-.128H9.2l-.79-.048-2.698-.073-2.339-.097-2.266-.122-.571-.121L0 11.784l.055-.352.48-.321.686.06 1.52.103 2.278.158 1.652.097 2.449.255h.389l.055-.157-.134-.098-.103-.097-2.358-1.596-2.552-1.688-1.336-.972-.724-.491-.364-.462-.158-1.008.656-.722.881.06.225.061.893.686 1.908 1.476 2.491 1.833.365.304.145-.103.019-.073-.164-.274-1.355-2.446-1.446-2.49-.644-1.032-.17-.619a2.97 2.97 0 01-.104-.729L6.283.134 6.696 0l.996.134.42.364.62 1.414 1.002 2.229 1.555 3.03.456.898.243.832.091.255h.158V9.01l.128-1.706.237-2.095.23-2.695.08-.76.376-.91.747-.492.584.28.48.685-.067.444-.286 1.851-.559 2.903-.364 1.942h.212l.243-.242.985-1.306 1.652-2.064.73-.82.85-.904.547-.431h1.033l.76 1.129-.34 1.166-1.064 1.347-.881 1.142-1.264 1.7-.79 1.36.073.11.188-.02 2.856-.606 1.543-.28 1.841-.315.833.388.091.395-.328.807-1.969.486-2.309.462-3.439.813-.042.03.049.061 1.549.146.662.036h1.622l3.02.225.79.522.474.638-.079.485-1.215.62-1.64-.389-3.829-.91-1.312-.329h-.182v.11l1.093 1.068 2.006 1.81 2.509 2.33.127.578-.322.455-.34-.049-2.205-1.657-.851-.747-1.926-1.62h-.128v.17l.444.649 2.345 3.521.122 1.08-.17.353-.608.213-.668-.122-1.374-1.925-1.415-2.167-1.143-1.943-.14.08-.674 7.254-.316.37-.729.28-.607-.461-.322-.747.322-1.476.389-1.924.315-1.53.286-1.9.17-.632-.012-.042-.14.018-1.434 1.967-2.18 2.945-1.726 1.845-.414.164-.717-.37.067-.662.401-.589 2.388-3.036 1.44-1.882.93-1.086-.006-.158h-.055L4.132 18.56l-1.13.146-.487-.456.061-.746.231-.243 1.908-1.312-.006.006z"

-- Derive feedback from the current draft, rather than an expiring app toast.
modelSettingsStatus :: M.Model -> F.ModelDraft -> String
modelSettingsStatus m draft =
  let
    saving = case m.dialog of
      Just dialog -> M.dialogBusy dialog
      Nothing -> false
    sapiLabel id = "Sapi customization — " <> case A.find ((_ == id) <<< _.id) m.agents of
      Just sapi -> sapi.name
      Nothing -> id
    invalid = (if F.modelConfigValid draft draft.defaults then [] else ["Global settings"]) <>
      map sapiLabel (A.filter (\id -> not $ F.modelConfigValid draft $ F.previewConfig (F.SapiOverride id) draft) $ O.keys draft.overrides)
  in
    if saving then "Saving model settings…"
    else if not (A.null invalid) then "Cannot save yet. Check model availability and effort for: " <> S.joinWith "; " invalid <> "."
    else if F.modelDraftChanged draft then "Unsaved changes"
    else "Saved"

modelSettings :: M.Model -> F.ModelDraft -> V.Html
modelSettings m draft = V.div "model-settings"
  [V.p "model-intro" "Models for this corpora and its Sapis."
  ,V.p "model-preview-note" "Changes apply to new runs. Running work is unchanged."
  ,V.textNode "p" [V.cls "model-hint",V.attr "role" "status",V.attr "aria-live" "polite"] (modelSettingsStatus m draft)
  ,V.node "nav" [V.cls "model-page-navigation",V.label "Model settings pages"]
    [pageButton F.GlobalModels "Global settings",pageButton F.SapiModels "Sapi customization"]
  ,case draft.page of
    F.GlobalModels -> V.node "section" [V.label "Global settings"]
      [V.div "model-section-heading" [V.textNode "h3" [] "Default provider",V.span [] $ D.fallback "Current corpora" m.snapshot.corpora.name]
      ,configFields F.CorporaDefault "Default" draft.defaults]
    F.SapiModels -> V.node "section" [V.label "Sapi customization"]
      [V.p "model-hint" "Choose a Sapi."
      ,if A.null sapis then V.p "model-hint" "No active Sapis in this corpora." else V.div "model-sapi-fields"
        [picker "model-sapi" "Sapi" draft.sapi false (map (\a -> {value:a.id,label:a.name}) sapis) F.SelectModelSapi
        ,V.div "model-inheritance-card"
          [checkbox "Use corpora default" inherited $ F.InheritModel draft.sapi
          ,V.p "model-hint" $ if inherited then "Follows global settings." else "Applies only to this Sapi."
          ,V.p "model-effective-summary" $ "Global default: " <> configSummary draft.defaults]
        ,if inherited then V.text "" else V.div "model-sapi-override"
          [V.div "model-section-heading" [V.textNode "h3" [] "Settings for this Sapi",V.span [V.cls "model-scope-badge"] "Override"]
          ,configFields (F.SapiOverride draft.sapi) "Sapi" (F.previewConfig (F.SapiOverride draft.sapi) draft)]]]
  ]
  where
  sapis = A.filter (\a -> not (M.isGroup a || a.retired)) m.agents
  inherited = not $ O.member draft.sapi draft.overrides
  configSummary config =
    let resolved = F.resolvedModelConfig (F.providerCatalog config.provider draft) config
        label = case A.find ((_ == resolved.model) <<< _.model) (F.catalogRows $ F.providerCatalog config.provider draft) of
          Just row -> row.label
          Nothing -> resolved.model
    in F.modelProviderName config.provider <> " · " <> label <> " · " <> if resolved.effort == "" then "Provider default effort" else resolved.effort
  act = DialogCommand <<< Intent.PreviewModel
  pageButton page caption = V.control
    [V.cls "model-page-button",V.attr "aria-current" $ if draft.page == page then "page" else "false"] caption (act $ F.ShowModelSettingsPage page)
  picker id caption value disabled options choose =
    let expanded = draft.picker == id
        selected = fromMaybe 0 $ A.findIndex ((_ == value) <<< _.value) options
        label = maybeLabel $ A.find ((_ == value) <<< _.value) options
        labels = map _.label options
        option index row = V.controlWith
          [V.id $ id <> "-option-" <> show index,V.cls "model-picker-option",V.attr "role" "option"
          ,V.label row.label,V.attr "aria-selected" $ show $ row.value == value,V.attr "tabindex" "-1"
          ,V.on "keydown" $ \event -> ModelPickerKey event id index labels]
          [V.span [] row.label,V.span [V.cls "model-picker-check",V.attr "aria-hidden" "true"] $ if row.value == value then "✓" else ""]
          (DialogCommand $ Intent.PickModelOption id $ choose row.value)
    in V.div "model-picker"
      [V.textNode "label" [V.attr "for" id] caption
      ,V.controlWith [V.id id,V.cls "model-picker-trigger",V.label $ caption <> ": " <> label,V.disabled disabled
        ,V.attr "aria-haspopup" "listbox",V.attr "aria-expanded" $ show expanded,V.attr "aria-controls" $ id <> "-list"
        ,V.on "keydown" $ \event -> ModelPickerKey event id (-1) labels]
        [V.span [] label,V.chevron false] (DialogCommand $ Intent.ToggleModelPicker id selected)
      ,V.when expanded $ V.div "model-picker-popover"
        [V.control [V.cls "model-picker-dismiss",V.attr "tabindex" "-1",V.label "Dismiss choices"] "" (DialogCommand $ Intent.ToggleModelPicker id selected)
        ,V.node "div" [V.id $ id <> "-list",V.cls "model-picker-list",V.attr "role" "listbox",V.label caption] $ A.mapWithIndex option options]]
  maybeLabel = case _ of
    Just option -> option.label
    Nothing -> "Choose a model"
  checkbox caption checked action = V.node "label" [V.cls "model-check"]
    [V.node "input" [V.attr "type" "checkbox",V.label caption,V.bool "checked" checked,V.check $ act <<< action] [],V.text caption]
  configFields target prefix config =
    let edit = act <<< F.EditModel target
        custom = case config.choice of
          F.RecommendedModel -> false
          _ -> true
        providerCard provider = V.node "label" [V.cls "model-provider"]
          [V.node "input" [V.attr "type" "radio",V.attr "name" $ prefix <> "-provider",V.label $ prefix <> " provider " <> F.modelProviderName provider,V.bool "checked" $ config.provider == provider,V.check $ \checked -> if checked then edit (F.ChooseProvider provider) else edit (F.ChooseProvider config.provider)] []
          ,V.node "span" [] [V.node "strong" [V.cls "model-provider-title"] [providerIcon provider,V.text $ F.modelProviderName provider]]]
    in V.div "model-config"
      [V.node "fieldset" [V.cls "model-providers",V.label $ prefix <> " provider"] $ map providerCard [F.Codex,F.Claude]
      ,case F.providerCatalog config.provider draft of
        F.CatalogLoading -> V.textNode "p" [V.cls "model-hint",V.attr "role" "status"] ("Loading " <> F.modelProviderName config.provider <> " models…")
        F.CatalogFailed failure -> V.div "model-hint" [V.textNode "p" [V.attr "role" "alert"] failure,V.button "button" "Retry model discovery" $ DialogCommand $ Intent.ReloadModels config.provider]
        F.CatalogReady rows | A.null rows -> V.div "model-hint" [V.p "" (F.modelProviderName config.provider <> " returned no visible models."),V.button "button" "Retry model discovery" $ DialogCommand $ Intent.ReloadModels config.provider]
        F.CatalogReady rows -> V.div ""
          [V.div "model-section-heading" [V.textNode "h3" [] "Model & effort",checkbox ("Customize " <> S.toLower prefix <> " model") custom $ F.EditModel target <<< F.CustomizeModel]
          ,case config.choice of
            F.RecommendedModel -> V.div "model-recommended"
              [V.span [] $ if config.provider == F.Codex then "Recommended: Astra · medium" else "Recommended: Opus 5.5 · medium"
              ,V.when (not $ A.any (\row -> row.model == F.recommendedModel config.provider && A.elem "medium" row.efforts) rows) $ V.p "model-hint" "The recommended model or effort is unavailable. Choose another model before saving."]
            F.CustomModel modelName effort -> V.div "model-custom-fields"
              [picker (S.toLower prefix <> "-model") (prefix <> " model") modelName false (map (\row -> {value:row.model,label:row.label}) rows) (F.EditModel target <<< F.ModelName)
              ,case A.find ((_ == modelName) <<< _.model) rows of
                Nothing -> V.p "model-hint" "Choose an available model."
                Just row -> picker (S.toLower prefix <> "-effort") (prefix <> " effort") (if effort == F.RecommendedEffort then "" else F.modelEffortName effort) (A.null row.efforts)
                  ([{value:"",label:if row.defaultEffort == "" then "Provider default" else "Model default (" <> row.defaultEffort <> ")"}] <> map (\value -> {value,label:value}) row.efforts)
                  (\value -> F.EditModel target $ F.ChooseEffort $ if value == "" then F.RecommendedEffort else F.CatalogEffort value)
              ]
          ]]

relayList :: M.Dialog -> V.Html
relayList d = if d.content == F.NoContent then V.p "" "Loading…" else V.div ""
  [V.node "div" [V.id "relay-list",V.cls $ "relay-list " <> if A.length rows >= 3 then "relay-scroll" else "",V.attr "role" "group",V.label "Relay servers"] $ map row rows
  ,V.node "label" [V.cls "relay-check"] [V.node "input" [V.attr "type" "checkbox",V.bool "checked" archived,V.check $ \b -> Field F.Archived $ show b] [],V.text "Show archived"]
  ,V.button "relay-add" "+ Add your own relay" $ DialogCommand Intent.AddRelay,V.textNode "small" [] "Recommended",V.p "relay-warning" "ⓘ Beta: use at your own risk. Self-host for sensitive work.",V.button "" (if (F.contextCorpora d.record).key /= "" then "Continue" else "Done") $ DialogCommand Intent.DoneRelay]
  where
  archived = d.fields.archived == "true"
  rows = A.filter (\r -> archived || not (r.archived)) $ (F.relays d.content).servers
  row r = let current = r.id == (F.relays d.content).selected in V.node "div" [V.cls "relay-row",V.attr "data-current" $ show current]
    [V.node "label" [] [V.node "input" [V.attr "type" "radio",V.attr "name" "relay-server",V.value $ r.id,V.bool "checked" current,V.disabled $ r.archived || not (r.available),V.check $ \_ -> DialogCommand $ Intent.SelectRelay r] [],V.node "span" [] [V.textNode "strong" [] (r.name),V.textNode "small" [] (D.replace "^https?://" "" $ r.address)]]
    ,V.span [V.cls "relay-badge"] (if r.archived then "Archived" else if not (r.available) then "Inactive" else if current then "Current" else "")
    ,V.div "relay-row-actions" [V.button "" "Edit" $ DialogCommand $ Intent.EditRelay r.id,V.button "" (if r.archived then "Restore" else "Archive") $ DialogCommand $ Intent.ArchiveRelay r]]

hostPairing :: M.Model -> M.Dialog -> V.Html
hostPairing m d = case d.content of
  F.Pairing status -> V.div "" [renderPair status, V.when (status.error /= "") $ V.p "" status.error]
  _ -> V.p "" "Loading…"
  where
  renderPair status = case F.pairView status of
    F.Unavailable -> V.p "" "Remote control is unavailable. Install the remote dependencies and restart Sapiens4."
    F.Paired device -> V.div "" [V.p "pairing-corpora" $ F.contextName d.record,V.node "dl" [V.cls "pairing-details"] $ A.concatMap (\(Tuple label text) -> [V.textNode "dt" [] label,V.textNode "dd" [] text]) [Tuple "Device" $ D.fallback "Not recorded" device.name,Tuple "Device info" $ D.fallback "Not recorded" $ S.joinWith " · " $ A.filter (_ /= "") [device.platform,device.client],Tuple "Paired" $ date "paired_at",Tuple "Last connected" $ date "last_connected_at"],V.div "pairing-actions" [action Intent.ReplaceHost "Replace remote",action Intent.RevokeHost "Revoke remote"]]
    F.AwaitingApproval pending -> V.div "" [V.p "" "Approve only if this code matches your remote device.",V.textNode "strong" [] pending.confirmation_code,V.control [V.disabled $ pending.confirmation_code == "" || M.dialogBusy d] "Approve device" (DialogCommand $ Intent.HostPair $ Intent.ApproveHost pending.peer)]
    F.Invited invitation
      | invitation.expires > m.now/1000.0 -> V.div "" [V.p "" "In the installed app, choose Connect remote corpora and scan this QR.",V.node "img" [V.id "host-pair-qr",V.attr "alt" "Pairing QR",V.attr "src" d.fields.qr] [],V.p "" $ "Expires " <> B.formatDate "" D.empty d.fields.expiration,V.div "pairing-actions" [V.button "" "Show link" $ Field F.ShowLink "true",V.textNode "a" [V.id "download-pair-qr",V.attr "download" "sapiens4-pairing.png",V.attr "href" d.fields.qr] "Download QR",V.link invitation.relay "Open app ↗",action Intent.RevokeHost "Cancel invitation"],V.node "input" [V.id "host-pair-link",V.label "Pairing link",V.bool "readOnly" true,V.hidden $ d.fields.showLink /= "true",V.value $ Remote.invitationLink invitation] []]
      | otherwise -> V.div "" [V.p "" "Invitation expired.",action Intent.RevokeHost "Cancel invitation"]
    F.Unpaired -> V.div "pairing-actions" [action Intent.InviteHost "Show QR",V.button "" "Relay servers" $ OpenDialog M.RelayList d.record]
  date key = D.fallback "Not recorded" $ fromMaybe "" $ O.lookup key d.fields.dates
  action key label = V.control [V.disabled $ M.dialogBusy d] label (DialogCommand $ Intent.HostPair key)

updateField :: F.Field -> String -> M.Dialog -> M.Dialog
updateField key value d =
  let fields = if key == F.Node && d.kind == M.DecisionPrompts then fromMaybe d.fields $ (\p -> F.promptFields p d.fields) <$> A.find (( _ == value) <<< _.node) (F.prompts d.content) else F.update key value d.fields
      withLead = if key == F.Lead && d.kind == M.GroupSettings then fields {members=A.nub $ A.snoc fields.members value} else fields
  in (M.setDialogError "" d) {fields=withLead}

orchestration :: M.Model -> String -> X.Orchestration
orchestration m id = fromMaybe (X.orchestration D.empty) $ O.lookup id m.snapshot.orchestration

open :: M.Dialog -> M.Model -> M.Model
open d m = m {dialog=Just d,dialogStack=[],menu=Nothing,sequence=d.identity,mention=Nothing}

close :: M.Model -> M.Model
close m = m {dialog=Nothing,dialogStack=[]}

push :: M.Dialog -> M.Model -> M.Model
push next m = m {dialog=Just $ next {content=fromMaybe F.NoContent $ _.content <$> m.dialog},dialogStack=m.dialogStack <> fromMaybe [] (map A.singleton m.dialog),sequence=next.identity}

back :: M.Model -> M.Model
back m = case A.unsnoc m.dialogStack of
  Nothing -> close m
  Just {init,last} -> let next = last {identity=m.sequence+1} in m {dialog=Just next,dialogStack=init,sequence=next.identity}

type SaveRequest = {path :: String, method :: String, body :: Json, caption :: String}

saveEntity :: M.Dialog -> Either String SaveRequest
saveEntity d = case d.kind of
  M.AgentSettings
    | not (D.matches "^[A-Z][A-Za-z0-9_.:#+|()&$^\\-]*$" d.fields.name) -> Left nameHelp
    | otherwise -> Right {path:"/api/agents/" <> B.encodeURI (F.contextId d.record),method:"PUT",body:D.json {name:d.fields.name,role:d.fields.role,manager:if d.fields.manager == "" then D.null else D.text d.fields.manager,execution:{mode:d.fields.mode}},caption:"Sapi saved."}
  M.GroupSettings -> Right {path:"/api/groups/" <> B.encodeURI (F.contextId d.record),method:"PUT",body:D.json {name:d.fields.name,description:d.fields.description,lead:d.fields.lead,members:d.fields.members,revision:d.fields.revision},caption:"Group saved."}
  _ -> Left "This dialog does not edit a Sapi or Group."

saveTask :: M.Model -> M.Dialog -> Boolean -> SaveRequest
saveTask m d deleted =
  let existing = F.contextId d.record /= ""
      payload = D.merge (D.json {title:d.fields.title,body:d.fields.body,assignee:d.fields.assignee,state:d.fields.state}) $ if existing then D.json {revision:D.json $ F.contextRevision d.record,deleted,state:if F.contextDeleted d.record then "backlog" else d.fields.state} else D.empty
  in {path:M.ownerPath m <> "/tasks" <> if existing then "/" <> B.encodeURI (F.contextId d.record) else "",method:if existing then "PUT" else "POST",body:payload,caption:if deleted then "Task deleted." else "Task saved."}
