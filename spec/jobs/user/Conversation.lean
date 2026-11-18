import io.Host

open Lean
namespace Spec.User

def resumeConversationAfterRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let question := "Review the café opening plan."
  let draft := "Remember the accessibility review."
  let (owner, turnId) ← withHost directory fun host => do
    let owner ← host.agent "Reviewer"
    let turn ← host.completedMessage owner question
    let turnId ← IO.ofExcept <| stringField turn "id"
    discard <| host.api "PUT" "/api/preferences"
      (json% {selected: $(owner), drafts: $(Json.mkObj [(owner, toJson draft)])})
    return (owner, turnId)
  withHost directory fun host => do
    let state ← host.state
    IO.ofExcept do
      let turns ← (← state.getObjVal? "turns").getArr?
      requireThat "restart duplicated the review" (turns.size == 1)
      let turn ← uniqueRecord (.arr turns) "id" (toJson turnId)
      fieldsMatch turn (json% {input: $(question), output: "The review is ready.", status: "done"})
      let preferences ← state.getObjVal? "preferences"
      fieldsMatch preferences (json% {selected: $(owner)})
      fieldsMatch (← preferences.getObjVal? "drafts") (Json.mkObj [(owner, toJson draft)])

def retainPrivateNotesAndBookmarks : IO Unit := IO.FS.withTempDir fun directory => do
  let marker := "Confidential review notes 901"
  let memo := s!"<section id=\"about\">Review</section><section id=\"map\">Index</section><section id=\"content\">{marker}</section>"
  let (owner, colleague, tabId, file) ← withHost directory fun host => do
    let owner ← host.chief
    let colleague ← host.agent "Reviewer"
    let status ← host.control owner (json% {op: "status"})
    let folder ← IO.ofExcept do stringField (← status.getObjVal? "notes") "workspace"
    let file := System.FilePath.mk folder / "review é.txt"
    IO.FS.writeFile (System.FilePath.mk folder / "Notes.html") memo
    IO.FS.writeFile file marker
    let opened ← host.control owner (json% {op: "workspace_open", path: $(file.toString)})
    let tabId ← IO.ofExcept do (← opened.getObjVal? "workspace").getObjVal? "active_tab"
    discard <| host.control owner (json% {op: "workspace_bookmark"})
    discard <| host.control owner (json% {op: "workspace_zoom", factor: 1.5})
    return (owner, colleague, tabId, file)
  withHost directory fun host => do
    let notes := fun agent => host.api "GET" ("/api/agents/" ++ agent ++ "/notes")
    IO.ofExcept <| fieldsMatch (← notes owner) (json% {content: $(memo)})
    IO.ofExcept <| requireThat "another agent received private notes" (!(← notes colleague).compress.contains marker)
    let workspace ← host.control owner (json% {op: "workspace"})
    IO.ofExcept do
      fieldsMatch workspace (json% {active_tab: $(tabId)})
      fieldsMatch (← uniqueRecord (← workspace.getObjVal? "tabs") "id" tabId) (json% {zoom: 1.5})
      discard <| uniqueRecord (← workspace.getObjVal? "bookmarks") "title" "review é.txt"
    discard <| host.control owner (json% {op: "workspace_close", id: $(tabId)})
    IO.ofExcept <| requireThat "closing the preview changed its source" ((← IO.FS.readFile file) == marker)
    let prompts := directory / "fixture-prompts"
    discard <| host.completedMessage owner "Recall my review notes"
    let files ← prompts.readDir
    let #[first] := files | throw <| IO.userError "Expected one owner prompt"
    IO.ofExcept <| requireThat "provider lost the saved notes" ((← IO.FS.readFile first.path).contains marker)
    IO.FS.removeFile first.path
    discard <| host.completedMessage colleague "Recall my review notes"
    let #[second] ← prompts.readDir | throw <| IO.userError "Expected one colleague prompt"
    IO.ofExcept <| requireThat "provider disclosed another owner's notes" (!(← IO.FS.readFile second.path).contains marker)

end Spec.User
