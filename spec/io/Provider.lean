import pure.ProviderReply

open Lean Spec

def main (args : List String) : IO UInt32 := do
  if args.contains "--version" then
    IO.println "codex-cli 0.158.0"
    return 0
  unless args.head? == some "exec" do throw <| IO.userError "Expected a model execution request"
  let prompt ← (← IO.getStdin).readToEnd
  let some folder ← IO.getEnv "SAPIENS_TEST_PROMPTS" | throw <| IO.userError "Missing fixture directory"
  let directory := System.FilePath.mk folder
  IO.FS.createDirAll directory
  let name := s!"{← IO.Process.getPID}-{← IO.monoNanosNow}"
  IO.FS.writeFile (directory / name) prompt
  if ← (directory / "fail-next").pathExists then
    IO.FS.removeFile (directory / "fail-next")
    IO.eprintln "Fixture provider unavailable"
    return 1
  while ← (directory / "hold").pathExists do IO.sleep 50
  let scriptPath := directory / ".." / "provider-decisions.json"
  let script ← if ← scriptPath.pathExists then IO.ofExcept <| Json.parse (← IO.FS.readFile scriptPath) else pure Json.null
  let reply ← IO.ofExcept <| providerReply prompt script
  IO.println (json% {type: "thread.started", thread_id: "fixture-session"}).compress
  IO.println (json% {type: "item.completed", item: {type: "agent_message", text: $(reply)}}).compress
  return 0
