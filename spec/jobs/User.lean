import support.Host

open Lean
namespace Spec.User

def resumeConversationAfterRestart : IO Unit := IO.FS.withTempDir fun directory => do
  let question := "Review the café opening plan."
  let draft := "Remember the accessibility review."
  let (owner, turnId) ← withHost directory fun host => do
    let owner ← host.agent "Reviewer"
    let submitted ← host.api "POST" ("/api/agents/" ++ owner ++ "/messages")
      (json% {text: $(question)}) 202
    let turnId ← IO.ofExcept <| stringField submitted "id"
    discard <| untilSome "a completed review" do
      let turns ← IO.ofExcept <| (← host.state).getObjVal? "turns"
      let turn ← IO.ofExcept <| uniqueRecord turns "id" (toJson turnId)
      let status ← IO.ofExcept <| stringField turn "status"
      if status == "failed" then throw <| IO.userError "Provider failed the review"
      return if status == "done" then some turn else none
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

def assignAndReviseGroupWork : IO Unit := IO.FS.withTempDir fun directory => do
  let (path, updated) ← withHost directory fun host => do
    let lead ← host.chief
    let assignee ← host.agent "Reviewer"
    let group ← host.api "POST" "/api/groups" (json% {name: "Launch", description: "Review readiness",
      lead: $(lead), members: [$(lead), $(assignee)]}) 201
    let path := "/api/groups/" ++ (← IO.ofExcept <| stringField group "id")
    let task ← host.api "POST" (path ++ "/tasks") (json% {title: "Review consent", assignee: $(assignee)}) 201
    let taskPath := path ++ "/tasks/" ++ (← IO.ofExcept <| stringField task "id")
    let revision ← IO.ofExcept <| task.getObjVal? "revision"
    let updated ← host.api "PUT" taskPath (json% {revision: $(revision), state: "done"})
    discard <| host.api "PUT" taskPath (json% {revision: $(revision), title: "Obsolete title"}) 409
    IO.ofExcept <| fieldsMatch updated (json% {title: "Review consent", assignee: $(assignee), state: "done"})
    return (path, updated)
  withHost directory fun host => do
    IO.ofExcept <| fieldsMatch (← host.api "GET" path) (json% {tasks: [$(updated)]})

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
