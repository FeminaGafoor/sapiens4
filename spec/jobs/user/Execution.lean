import io.Host

open Lean System
namespace Spec.User

private def prepareReview (directory : FilePath) (overrides := Json.mkObj []) : IO Unit := do
  let script := json% {
    Assessing: {event: "FitsSpecialization", mode: "Exec"},
    ReadyToWork: {event: "TaskPrepared", taskType: "research", specification: {
      objective: "Review release readiness", inputs: [], expectedOutputs: ["Report"], completionCriteria: ["Record findings"]}},
    Execution: {event: "Outcome", reply: "Review complete", outcome: {
      status: "Completed", satisfiedCriteria: ["Record findings"], artifacts: []}}}
  let rules ← IO.ofExcept overrides.getObj?
  IO.FS.writeFile (directory / "provider-decisions.json") ((rules.toList.foldl
    (fun result entry => result.setObjVal! entry.1 entry.2) script).compress)

private def taskFor (host : Host) (owner : String) (turnId : Json) : IO Json :=
  untilSome "review completion" do
    let result ← host.api "GET" ("/api/agents/" ++ owner ++ "/tasks?format=json")
    let tasks ← IO.ofExcept <| result.getObjVal? "tasks"
    let .ok task := uniqueRecord tasks "id" turnId | return none
    let state ← IO.ofExcept <| stringField task "state"
    if state == "Failed" then throw <| IO.userError s!"Review failed: {task.getObjValD "error"}"
    return if state == "Completed" then some task else none

def delegateOneAccountableReview : IO Unit := IO.FS.withTempDir fun directory => do
  let (chief, turnId, completed) ← withHost directory fun host => do
    let chief ← host.chief
    let reviewer ← host.agent "Reviewer"
    let routes := Json.mkObj [
      (chief, json% {Assessing: {event: "OutsideSpecialization"}}),
      ("ChiefTriage", json% {event: "SpecialistSelected", target: $(reviewer)}),
      ("Delegation", json% {event: "HandoffPrepared", request: "Review release readiness"})]
    prepareReview directory routes
    let turnId ← host.submit chief "Review release readiness"
    let task ← taskFor host chief turnId
    let state ← host.state
    IO.ofExcept do
      fieldsMatch task (json% {sender: $(chief), owner: $(reviewer), result: "Review complete"})
      let work ← uniqueRecord (← state.getObjVal? "workloads") "id" turnId
      fieldsMatch work (json% {participants: [$(chief), $(reviewer)], calls: 2})
    return (chief, turnId, task)
  withHost directory fun host => do
    IO.ofExcept <| requireThat "restart changed the delegated review" ((← taskFor host chief turnId) == completed)

def finishOneScheduledReview : IO Unit := IO.FS.withTempDir fun directory => do
  prepareReview directory
  let (owner, routineId, task) ← withHost directory fun host => do
    let owner ← host.chief
    let notesPath := "/api/agents/" ++ owner ++ "/notes"
    let memo ← IO.ofExcept <| (← host.api "GET" notesPath).getObjVal? "content"
    let result ← host.control owner (json% {op: "routine_create", minutes: 1, prompt: "Review release readiness"})
    let routineId ← IO.ofExcept do (← result.getObjVal? "routine").getObjVal? "id"
    let turnId ← untilSome "the real minute scheduler to admit the review" (do
      let turns ← IO.ofExcept <| (← host.state).getObjValAs? (Array Json) "turns"
      let due := turns.filter fun turn => (turn.getObjValD "origin").getObjValD "routine" == routineId
      if due.isEmpty then return none
      let #[turn] := due | throw <| IO.userError "The scheduler admitted duplicate work"
      return some (← IO.ofExcept <| turn.getObjVal? "id")) 150000 500
    discard <| host.control owner (json% {op: "routine_pause", id: $(routineId)})
    let task ← taskFor host owner turnId
    IO.ofExcept <| fieldsMatch task (json% {taskType: "automated", owner: $(owner), result: "Review complete"})
    IO.ofExcept <| fieldsMatch (← host.api "GET" notesPath) (json% {content: $(memo)})
    return (owner, routineId, task)
  withHost directory fun host => do
    let state ← host.state
    IO.ofExcept do
      let routine ← uniqueRecord (← state.getObjVal? "routines") "id" routineId
      fieldsMatch routine (json% {paused: true, runCount: 1})
    let tasks ← host.api "GET" ("/api/agents/" ++ owner ++ "/tasks?format=json")
    IO.ofExcept <| fieldsMatch tasks (json% {tasks: [$(task)]})

end Spec.User
