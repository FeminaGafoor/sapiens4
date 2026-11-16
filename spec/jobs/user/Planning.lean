import io.Host

open Lean
namespace Spec.User

def keepScheduledWorkPausedUntilItsOwnerResumes : IO Unit := IO.FS.withTempDir fun directory => do
  let (owner, routineId, paused) ← withHost directory fun host => do
    let owner ← host.chief
    let colleague ← host.agent "Planner"
    let created ← host.control owner (json% {op: "routine_create", minutes: 60, prompt: "Review the plan"})
    let routineId ← IO.ofExcept do stringField (← created.getObjVal? "routine") "id"
    let result ← host.control owner (json% {op: "routine_pause", id: $(routineId)})
    let paused ← IO.ofExcept <| result.getObjVal? "routine"
    IO.ofExcept <| fieldsMatch paused (json% {paused: true, runCount: 0})
    discard <| host.control colleague (json% {op: "routine_resume", id: $(routineId)}) 404
    return (owner, routineId, paused)
  withHost directory fun host => do
    IO.ofExcept <| fieldsMatch (← host.state) (json% {routines: [$(paused)]})
    let resumed ← host.control owner (json% {op: "routine_resume", id: $(routineId)})
    IO.ofExcept do fieldsMatch (← resumed.getObjVal? "routine") (json% {paused: false, runCount: 0})

end Spec.User
