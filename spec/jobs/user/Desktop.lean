import io.Desktop

open Lean
namespace Spec.User

def pauseAndResumeFromTheDesktop : IO Unit := IO.FS.withTempDir fun directory => do
  withDesktop directory fun desktop => do
    desktop.clickButton "Active"
    desktop.clickButton "Pause"
    discard <| desktop.button "Paused"
  withHost directory fun host => do
    IO.ofExcept <| fieldsMatch (← host.api "GET" "/api/corpora") (json% {status: "paused"})
  withDesktop directory fun desktop => do
    desktop.clickButton "Paused"
    desktop.clickButton "Activate"
    discard <| desktop.button "Active"
  withHost directory fun host => do
    IO.ofExcept <| fieldsMatch (← host.api "GET" "/api/corpora") (json% {status: "active"})

def sendAndReadOneDesktopReply : IO Unit := IO.FS.withTempDir fun directory => do
  let message := if System.Platform.isWindows || System.Platform.isOSX then
    "Review the café plan from my desktop" else "Review the cafe plan from my desktop"
  withDesktop directory fun desktop => do
    let composer := untilSome "the ChiefSapi message editor" do
      let areas ← desktop.findRole "AXTextArea"
      let fields ← desktop.findRole "AXTextField"
      let candidates := (areas ++ fields).toList.eraseDups.toArray
      IO.ofExcept <| enabledControl candidates "Message ChiefSapi"
    let editor ← composer
    let target ← IO.ofExcept <| stringField editor "path"
    discard <| desktop.invoke "paste" #["--target-path", target, "--text", message]
    let editor ← composer
    IO.ofExcept <| fieldsMatch editor (json% {value: $(message)})
    let path ← IO.ofExcept <| stringField editor "path"
    discard <| desktop.invoke "key" #["--key", "return", "--target-path", path, "--require-value", message]
    discard <| untilSome "the assistant's reply in the conversation" do
      let nodes ← desktop.findRole "AXStaticText"
      return if nodes.any (fun node => ["title", "value", "description"].any
        (fun key => ((stringField node key).toOption.getD "").contains "The review is ready.")) then some () else none
  withHost directory fun host => do
    let turns ← IO.ofExcept <| (← host.state).getObjValAs? (Array Json) "turns"
    let #[turn] := turns | throw <| IO.userError "Expected one submitted desktop message"
    IO.ofExcept <| fieldsMatch turn (json% {input: $(message), output: "The review is ready.", status: "done"})

end Spec.User
