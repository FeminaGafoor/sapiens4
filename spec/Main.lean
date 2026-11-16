import jobs.User
import support.Provider

open Spec

def main (args : List String) : IO UInt32 := do
  if args.contains "--version" || args.head? == some "exec" then return ← runFixtureProvider args
  User.resumeConversationAfterRestart
  IO.println "PASS Resume a conversation with its reply and draft after restart"
  User.assignAndReviseGroupWork
  IO.println "PASS Finish assigned work without losing it to a stale edit"
  User.keepScheduledWorkPausedUntilItsOwnerResumes
  IO.println "PASS Keep scheduled work paused until its owner resumes it"
  return 0
