import io.Host

open Lean
namespace Spec.Founder

def retainControlOverScopeAndSuspendedWork : IO Unit := IO.FS.withTempDir fun directory => do
  let metadata := fun (host : Host) => host.api "GET" "/api/corpora"
  let (active, pauseCommand) ← withHost directory fun host => do
    let chief ← host.chief
    let colleague ← host.agent "Planner"
    let initial ← metadata host
    let revision ← IO.ofExcept <| initial.getObjVal? "revision"
    let change := json% {op: "corpora_update", name: "Studio", scope: "Private review scope", revision: $(revision)}
    let refused ← request host.url "POST" ("/api/agents/" ++ colleague ++ "/control") change
    IO.ofExcept <| verifyDenial "only the chief can change scope" 403 refused initial (← metadata host) []
    let updated ← host.control chief change
    IO.ofExcept <| fieldsMatch updated (json% {name: "Studio", scope: "Private review scope"})
    let revision ← IO.ofExcept <| updated.getObjVal? "revision"
    let pauseCommand := json% {action: "pause", revision: $(revision), operation: "8aa103a9-56bb-492d-aa30-1ae30f837071"}
    let paused ← host.api "PUT" "/api/corpora/lifecycle" pauseCommand
    let turns := do IO.ofExcept <| (← host.state).getObjVal? "turns"
    let before ← turns
    let denied ← request host.url "POST" ("/api/agents/" ++ chief ++ "/messages") (json% {text: "Start budget review"})
    IO.ofExcept <| verifyDenial "suspended work" 409 denied before (← turns) ["Private review scope"]
    let revision ← IO.ofExcept <| paused.getObjVal? "revision"
    let active ← host.api "PUT" "/api/corpora/lifecycle"
      (json% {action: "activate", revision: $(revision), operation: "0f92546f-143a-41d5-be6a-80a84a10e7b9"})
    return (active, pauseCommand)
  withHost directory fun host => do
    let replay ← host.api "PUT" "/api/corpora/lifecycle" pauseCommand
    let persisted ← metadata host
    let state ← host.state
    IO.ofExcept do
      fieldsMatch replay (json% {status: "active", revision: $(← active.getObjVal? "revision")})
      fieldsMatch persisted (json% {status: "active", scope: "Private review scope", revision: $(← active.getObjVal? "revision")})
      fieldsMatch (← state.getObjVal? "pulse") (json% {running: true})

end Spec.Founder
