import jobs.User
import jobs.System
import jobs.Founder
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
  SystemJobs.rejectUntrustedRequestsWithoutDisclosingLocalState
  IO.println "PASS Reject untrusted reads and writes without changing the roster"
  SystemJobs.keepFilePreviewsAndAttachmentsWithTheirOwner
  IO.println "PASS Enforce file and attachment ownership after restart"
  Founder.retainControlOverScopeAndSuspendedWork
  IO.println "PASS Retain authority over scope and lifecycle across retries"
  Founder.approveOneDeviceThenRevokeItsMailbox
  IO.println "PASS Approve one remote device, reconnect, and revoke its access"
  SystemJobs.keepGroupBriefsSeparateFromPersonalConversations
  IO.println "PASS Keep private chats and group briefs separate at the provider"
  SystemJobs.eraseCancelledVoiceDraftsAcrossRestart
  IO.println "PASS Erase cancelled audio and refuse stale device retries"
  return 0
