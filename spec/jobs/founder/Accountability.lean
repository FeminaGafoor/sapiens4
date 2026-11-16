import io.Host

open Lean
namespace Spec.Founder

def reviewFailedWorkBeforeRetry : IO Unit := IO.FS.withTempDir fun directory => do
  let prompts := directory / "fixture-prompts"
  IO.FS.createDirAll prompts
  IO.FS.writeFile (prompts / "fail-next") ""
  let input := "Review the launch costs"
  let (owner, turnId, audit) ← withHost directory fun host => do
    let owner ← host.chief
    let turnId ← host.submit owner input
    discard <| host.awaitTurn turnId "failed"
    let id ← IO.ofExcept turnId.getStr?
    let auditFile := directory / "agentpy/corpora/archive" / owner / "runs" / (id ++ ".json")
    let record ← IO.ofExcept <| Json.parse (← IO.FS.readFile auditFile)
    IO.ofExcept do
      let message ← stringField record "error"
      requireThat "provider failure missing from the audit" (message.contains "Fixture provider unavailable")
      let calls ← record.getObjValAs? (Array Json) "logs"
      let #[call] := calls | throw "Expected one failed assessment"
      fieldsMatch call (json% {role: "Assessing"})
      requireThat "the reviewed request is missing" ((← stringField call "prompt").contains input)
    return (owner, turnId, (auditFile, record))
  withHost directory fun host => do
    discard <| host.awaitTurn turnId "failed"
    let persisted ← IO.ofExcept <| Json.parse (← IO.FS.readFile audit.1)
    IO.ofExcept <| requireThat "restart changed the failed audit" (persisted == audit.2)
    IO.ofExcept <| requireThat "restart repeated the failed call" ((← prompts.readDir).size == 1)
    discard <| host.api "POST" ("/api/agents/" ++ owner ++ "/turns/" ++ (← IO.ofExcept turnId.getStr?) ++ "/retry") (json% {})
    let completed ← host.awaitTurn turnId "done"
    IO.ofExcept <| fieldsMatch completed (json% {input: $(input), output: "The review is ready."})
    IO.ofExcept <| requireThat "explicit retry should make one call" ((← prompts.readDir).size == 2)

end Spec.Founder
