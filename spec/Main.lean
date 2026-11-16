import jobs.user.Desktop
import jobs.user.Execution
import jobs.user.Conversation
import jobs.user.Planning
import jobs.system.Access
import jobs.system.PrivateData
import jobs.system.Recovery
import jobs.founder.Governance
import jobs.founder.DeviceConsent
import jobs.founder.Accountability

open Lean Spec

abbrev Job := String × IO Unit

def headlessJobs : List Job := [
  ("Process redelivered encrypted remote work once across restart", Founder.processRedeliveredRemoteWorkOnce),
  ("Restore private notes and bookmarks without disclosing them", User.retainPrivateNotesAndBookmarks),
  ("Complete one scheduled review on the real minute pulse", User.finishOneScheduledReview),
  ("Delegate one review and preserve its owner and result", User.delegateOneAccountableReview),
  ("Migrate interrupted work without replaying it or replacing the backup", SystemJobs.preserveInterruptedWorkDuringUpgrade),
  ("Recover saved work after storage repair under a single host", SystemJobs.recoverSavedWorkAfterStorageRepair),
  ("Release exclusive computer access when its owner is cancelled", SystemJobs.releaseExclusiveComputerAccessOnCancellation),
  ("Review the failed assessment before explicitly retrying it", Founder.reviewFailedWorkBeforeRetry),
  ("Resume a conversation with its reply and draft after restart", User.resumeConversationAfterRestart),
  ("Keep scheduled work paused until its owner resumes it", User.keepScheduledWorkPausedUntilItsOwnerResumes),
  ("Reject untrusted reads and writes without changing the roster", SystemJobs.rejectUntrustedRequestsWithoutDisclosingLocalState),
  ("Enforce file and attachment ownership after restart", SystemJobs.keepFilePreviewsAndAttachmentsWithTheirOwner),
  ("Retain authority over scope and lifecycle across retries", Founder.retainControlOverScopeAndSuspendedWork),
  ("Keep local corpora and their drafts separate after restart", SystemJobs.keepLocalCorporaSeparateAcrossRestart),
  ("Withdraw pairing consent before the other device approves", Founder.withdrawConsentBeforeTheOtherDeviceApproves),
  ("Approve one remote device, reconnect, and revoke its access", Founder.approveOneDeviceThenRevokeItsMailbox),
  ("Erase cancelled audio and refuse stale device retries", SystemJobs.eraseCancelledVoiceDraftsAcrossRestart)]

def desktopJobs : List Job := [
  ("Send one desktop message and read the persisted reply", User.sendAndReadOneDesktopReply),
  ("Pause and resume work from the desktop across relaunch", User.pauseAndResumeFromTheDesktop)]

def main (args : List String) : IO UInt32 := do
  let jobs ← match args with
    | [] => pure (headlessJobs ++ desktopJobs)
    | ["headless"] => pure headlessJobs
    | ["desktop"] => pure desktopJobs
    | _ => throw <| IO.userError "Usage: lake -d spec test [-- headless|desktop]"
  let mut failures := 0
  let mut examples : Array Json := #[]
  for (name, exercise) in jobs do
    try
      exercise
      IO.println s!"PASS {name}"
      examples := examples.push (json% {description: $(name), status: "passed"})
    catch error =>
      failures := failures + 1
      IO.eprintln s!"FAIL {name}: {error}"
      examples := examples.push (json% {description: $(name), status: "failed", message: $(toString error)})
  if let some path ← IO.getEnv "SAPIENS_TEST_RESULTS" then
    IO.FS.writeFile path (json% {examples: $(examples), summary: {example_count: $(jobs.length), failure_count: $(failures), pending_count: 0}}).compress
  IO.println s!"{jobs.length - failures}/{jobs.length} jobs passed"
  return if failures == 0 then 0 else 1
