import Lean.Data.Json

open Lean

def runFixtureProvider (args : List String) : IO UInt32 := do
  if args.contains "--version" then
    IO.println "codex-cli 0.158.0"
    return 0
  let prompt ← (← IO.getStdin).readToEnd
  let reply := if prompt.contains "decision at node Assessing" then
    (json% {event: "FitsSpecialization", mode: "Repl", reply: "The review is ready.",
      reason: "The request fits my role.", evidence: ["The review request"]}).compress
    else "The review is ready."
  IO.println (json% {type: "thread.started", thread_id: "fixture-session"}).compress
  IO.println (json% {type: "item.completed", item: {type: "agent_message", text: $(reply)}}).compress
  return 0
