import io.CryptoWindow

open Lean
namespace Spec.SystemJobs

def preserveBrowserApplicationBehavior : IO Unit := IO.FS.withTempDir fun directory =>
  withHost directory fun host => withBrowserFixture host fun desktop browser => do
    let check := fun (operation : String) => do
      IO.ofExcept <| requireThat operation ((← browser (json% {operation: $(operation)})) == toJson true)
    check "verify"
    check "application"
    check "setup"
    desktop.clickButton "Accept terms"
    check "accepted"
    desktop.clickButton "Use another folder"
    check "picking"
    check "picked"
    desktop.clickButton "Continue"
    check "submitted"
    check "error"
    desktop.clickButton "Accept terms"
    check "revoked"
    let link ← untilSome "Privacy Policy" do
      IO.ofExcept <| enabledControl (← desktop.findRole "AXLink") "Privacy Policy"
    desktop.pressControl link "Privacy Policy"
    check "legal"

end Spec.SystemJobs
