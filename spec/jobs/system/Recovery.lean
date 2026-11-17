import io.Host

open Lean
namespace Spec.SystemJobs

def preserveInterruptedWorkDuringUpgrade : IO Unit := IO.FS.withTempDir fun directory => do
  let owner ← withHost directory fun host => do
    let metadata ← host.api "GET" "/api/corpora"
    let revision ← IO.ofExcept <| metadata.getObjVal? "revision"
    discard <| host.api "PUT" "/api/corpora/lifecycle"
      (json% {action: "pause", operation: "pause-before-upgrade", revision: $(revision)})
    host.chief
  let agentDir := directory / "agentpy" / "agents" / owner
  let file := agentDir / "state.json"
  let installed ← IO.ofExcept <| Json.parse (← IO.FS.readFile file)
  let turn := json% {id: "saved-review", flow: "chat", input: "Review the proposal",
    status: "budget_blocked", attempt: 3, created: "2026-10-27T12:00:00+00:00"}
  let previous := ((installed.setObjVal! "schema_version" 2).setObjVal! "turns" (json% [$(turn)])).compress
  IO.FS.writeFile file previous
  for _ in [0:2] do
    withHost directory fun host => do
      let recovered ← host.awaitTurn "saved-review" "interrupted"
      IO.ofExcept <| fieldsMatch recovered (json% {input: "Review the proposal", status: "interrupted"})
      let upgraded ← IO.ofExcept <| Json.parse (← IO.FS.readFile file)
      IO.ofExcept do
        fieldsMatch upgraded (json% {schema_version: 3})
        let saved ← uniqueRecord (← upgraded.getObjVal? "turns") "id" "saved-review"
        fieldsMatch saved (json% {status: "interrupted", input: "Review the proposal"})
        requireThat "missing recovery explanation" ((← stringField saved "error").contains "retry")
      let backup ← IO.FS.readFile (agentDir / "legacy-state-v2.json")
      IO.ofExcept <| requireThat "original installation backup changed" (backup == previous)
      IO.ofExcept <| requireThat "upgrade ran a provider call" (!(← (directory / "fixture-prompts").pathExists))

def recoverSavedWorkAfterStorageRepair : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let owner ← host.chief
    let metadata ← host.api "GET" "/api/corpora"
    discard <| host.api "PUT" "/api/corpora/lifecycle"
      (json% {action: "pause", operation: "pause-before-repair", revision: $(metadata.getObjValD "revision")})
    let root ← repoRoot
    let contender ← IO.Process.output {
      cmd := (binary root "sapiens4").toString
      args := #["--desktop", "--port", "0", "--data-dir", directory.toString],
      cwd := root, env := ← fixtureEnvironment directory} (some "")
    IO.ofExcept <| requireThat "a second host failed to respect the data-directory lock"
      (contender.exitCode != 0 && contender.stderr.contains "Another Sapiens4 server")
    let file := directory / "agentpy" / "agents" / owner / "state.json"
    let saved ← IO.ofExcept <| Json.parse (← IO.FS.readFile file)
    let turn := json% {id: "saved-review", flow: "chat", input: "Original request", status: "failed",
      error: "Original error", created: "2026-10-27T12:00:00+00:00"}
    let install := fun (record : Json) => do
      let staging := file.addExtension "tmp"
      IO.FS.writeFile staging (saved.setObjVal! "turns" (json% [$(record)])).compress
      IO.FS.rename staging file
    install turn
    let initial ← host.awaitTurn "saved-review" "failed"
    install (turn.setObjVal! "status" "invalid")
    let failed ← request host.url "GET" "/api/state"
    let health ← request host.url "GET" "/api/health"
    IO.ofExcept <| requireThat "malformed saved work must fail the snapshot while the host remains available"
      (failed.status == 500 && health.status == 200)
    install turn
    let restored ← host.awaitTurn "saved-review" "failed"
    IO.ofExcept <| requireThat "repair changed the saved review" (restored == initial)
    IO.ofExcept <| requireThat "repair ran a provider call" (!(← (directory / "fixture-prompts").pathExists))

def releaseExclusiveComputerAccessOnCancellation : IO Unit :=
  IO.FS.withTempDir fun directory => withHost directory fun host => do
    let chief ← host.chief
    let colleague ← host.agent "Operator"
    let acquire := json% {op: "computer_acquire"}
    discard <| host.control chief acquire 409
    let prompts := directory / "fixture-prompts"
    IO.FS.createDirAll prompts
    IO.FS.writeFile (prompts / "hold") ""
    try
      let first ← host.submit chief "Review the workspace"
      let second ← host.submit colleague "Check the workspace"
      discard <| untilSome "both active provider calls" do
        let calls := (← prompts.readDir).filter (·.fileName != "hold")
        return if calls.size == 2 then some () else none
      IO.ofExcept <| fieldsMatch (← host.control chief acquire) (json% {owner: $(chief)})
      let denied ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/control") acquire
      let lease ← IO.ofExcept <| (← host.state).getObjVal? "computer"
      IO.ofExcept do
        requireThat "concurrent computer access was accepted" (denied.status == 409)
        fieldsMatch lease (json% {owner: $(chief)})
      discard <| host.api "POST" ("/api/agents/" ++ chief ++ "/turns/" ++ (← IO.ofExcept first.getStr?) ++ "/cancel") (json% {})
      discard <| untilSome "release of the cancelled turn's lease" do
        let state ← host.state
        let owner ← IO.ofExcept do (← state.getObjVal? "computer").getObjVal? "owner"
        return if owner == .null then some () else none
      IO.ofExcept <| fieldsMatch (← host.control colleague acquire) (json% {owner: $(colleague)})
      IO.FS.removeFile (prompts / "hold")
      discard <| host.awaitTurn second "done"
      let computer ← IO.ofExcept <| (← host.state).getObjVal? "computer"
      IO.ofExcept <| fieldsMatch computer (json% {owner: null})
    finally
      if ← (prompts / "hold").pathExists then IO.FS.removeFile (prompts / "hold")

end Spec.SystemJobs
