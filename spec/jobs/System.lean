import support.Host

open Lean
namespace Spec.SystemJobs

def rejectUntrustedRequestsWithoutDisclosingLocalState : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let marker := "PrivateRosterMarker"
    discard <| host.agent marker
    let roster := do IO.ofExcept <| (← host.state).getObjVal? "agents"
    let before ← roster
    IO.ofExcept <| requireThat "trusted caller must see the fixture" (before.compress.contains marker)
    for (name, headers) in [
      ("other origin", #["Origin: https://other.invalid", "Content-Type: application/json", "X-Sapiens-Local: 1"]),
      ("missing local intent", #["Content-Type: application/json"]),
      ("non-JSON write", #["Content-Type: text/plain", "X-Sapiens-Local: 1"])] do
      let reply ← request host.url "POST" "/api/agents" (json% {name: "Unexpected", role: "Research"}) headers
      IO.ofExcept <| verifyDenial name 403 reply before (← roster) [marker]
    for (name, path, headers, status) in [
      ("opaque origin", "/api/state", #["Origin: null"], 403),
      ("unrecognized host", "/api/state", #["Host: other.invalid"], 403),
      ("private database", "/corpora.sqlite3", #[], 404)] do
      let reply ← request host.url "GET" path none headers
      IO.ofExcept <| verifyDenial name status reply before (← roster) [marker]

def keepFilePreviewsAndAttachmentsWithTheirOwner : IO Unit := IO.FS.withTempDir fun directory => do
  let marker := "Private finance review 417"
  let (owner, colleague, tabId, attachmentId) ← withHost directory fun host => do
    let owner ← host.chief
    let colleague ← host.agent "Colleague"
    let status ← host.control owner (json% {op: "status"})
    let workspace ← IO.ofExcept do stringField (← status.getObjVal? "notes") "workspace"
    let file := System.FilePath.mk workspace / "finance.txt"
    IO.FS.writeFile file marker
    let opened ← host.control owner (json% {op: "workspace_open", path: $(file.toString)})
    let tabId ← IO.ofExcept do
      let tabs ← (← opened.getObjVal? "workspace").getObjValAs? (Array Json) "tabs"
      let #[tab] := tabs | throw "expected one open finance tab"
      stringField tab "id"
    let upload ← host.api "POST" ("/api/agents/" ++ owner ++ "/attachments")
      (json% {kind: "document", name: "finance.txt", data: "UHJpdmF0ZSBmaW5hbmNlIHJldmlldyA0MTc="}) 201
    let stored ← IO.ofExcept <| stringField upload "value"
    IO.ofExcept <| requireThat "uploaded document changed" ((← IO.FS.readFile stored) == marker)
    return (owner, colleague, tabId, ← IO.ofExcept <| stringField upload "id")
  withHost directory fun host => do
    let preview := fun agent => "/api/agents/" ++ agent ++ "/browser/" ++ tabId ++ "/content"
    let owned ← host.api "GET" (preview owner)
    IO.ofExcept <| fieldsMatch owned (json% {content: $(marker)})
    let turns := do IO.ofExcept <| (← host.state).getObjVal? "turns"
    let before ← turns
    let denied ← request host.url "GET" (preview colleague)
    IO.ofExcept <| verifyDenial "another agent's file" 404 denied before (← turns) [marker]
    let denied ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/messages")
      (json% {text: "Review document", attachments: [$(attachmentId)]})
    IO.ofExcept <| verifyDenial "another agent's attachment" 400 denied before (← turns) [marker]
    discard <| host.api "POST" ("/api/agents/" ++ owner ++ "/messages")
      (json% {text: "Review my document", attachments: [$(attachmentId)]}) 202
    IO.ofExcept <| fieldsMatch (← host.api "GET" (preview owner)) (json% {content: $(marker)})

private def takePrompts (directory : System.FilePath) : IO (List String) := do
  let mut prompts := []
  for entry in ← (directory / "fixture-prompts").readDir do
    prompts := (← IO.FS.readFile entry.path) :: prompts
    IO.FS.removeFile entry.path
  return prompts

def keepGroupBriefsSeparateFromPersonalConversations : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let personal := "PersonalMarker718"
    let shared := "GroupBriefMarker492"
    let followup := "PersonalFollowup351"
    let lead ← host.agent "Editor"
    let member ← host.agent "Planner"
    let group ← host.api "POST" "/api/groups" (json% {name: "Launch", description: "Prepare the release",
      lead: $(lead), members: [$(lead), $(member)]}) 201
    let path := "/api/groups/" ++ (← IO.ofExcept <| stringField group "id")
    discard <| host.completedMessage lead personal
    let personalCalls ← takePrompts directory
    IO.ofExcept <| requireThat "private prompt missing at provider" (personalCalls.any (·.contains personal))
    discard <| host.api "POST" (path ++ "/messages") (json% {text: $(shared)}) 202
    discard <| untilSome "the group and its memo to settle" do
      let group ← host.api "GET" path
      let requests ← IO.ofExcept <| group.getObjValAs? (Array Json) "requests"
      let settled ← IO.ofExcept <| requests.mapM (·.getObjValAs? Bool "settled")
      return if !requests.isEmpty && settled.all id then some () else none
    let groupCalls ← takePrompts directory
    IO.ofExcept do
      requireThat "group brief missing at provider" (groupCalls.any (·.contains shared))
      requireThat "group work disclosed a private conversation" (groupCalls.all (!·.contains personal))
    discard <| host.completedMessage lead followup
    let nextPrivateCalls ← takePrompts directory
    IO.ofExcept do
      requireThat "follow-up missing at provider" (nextPrivateCalls.any (·.contains followup))
      requireThat "personal work disclosed the group brief" (nextPrivateCalls.all (!·.contains shared))

def eraseCancelledVoiceDraftsAcrossRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let recording := "a82a45dc-8885-4965-82fe-d1ad007a9c74"
  let silentSecond := "UklGRiR9AABXQVZFZm10IBAAAAABAAEAgD4AAAB9AAACABAAZGF0YQB9" ++
    String.ofList (List.replicate 42670 'A') ++ "=="
  let piece := fun (index : Nat) => json% {recording: $(recording), index: $(index), final: true, data: $(silentSecond)}
  let (owner, clip, file) ← withHost directory fun host => do
    let owner ← host.chief
    let path := "/api/agents/" ++ owner ++ "/voice"
    let saved ← host.api "POST" path (piece 0)
    let messages ← IO.ofExcept <| saved.getObjValAs? (Array Json) "messages"
    let #[message] := messages | throw <| IO.userError "Expected a single voice message"
    let clip ← IO.ofExcept <| stringField message "id"
    let file := System.FilePath.mk (← IO.ofExcept <| stringField message "value")
    IO.ofExcept <| requireThat "voice data missing before cancellation" (← file.pathExists)
    let drafts := Json.mkObj [(owner, toJson [clip])]
    discard <| host.api "PUT" "/api/preferences" (json% {attachment_drafts: $(drafts)})
    let cancelled ← host.api "POST" (path ++ "/" ++ recording ++ "/cancel") (json% {})
    IO.ofExcept <| fieldsMatch cancelled (json% {cancelled: [$(clip)]})
    return (owner, clip, file)
  withHost directory fun host => do
    let path := "/api/agents/" ++ owner
    discard <| host.api "POST" (path ++ "/voice") (piece 1) 409
    discard <| host.api "POST" (path ++ "/messages") (json% {text: "", attachments: [$(clip)]}) 409
    discard <| host.api "PUT" "/api/preferences"
      (json% {attachment_drafts: $(Json.mkObj [(owner, toJson [clip])])})
    let state ← host.state
    let existsNow ← file.pathExists
    IO.ofExcept do
      requireThat "cancelled audio remains on disk" (!existsNow)
      let drafts ← state.getObjVal? "attachment_drafts"
      requireThat "a stale device restored the draft" (!drafts.compress.contains clip)
      fieldsMatch state (json% {turns: []})

end Spec.SystemJobs
