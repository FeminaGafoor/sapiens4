import Lean.Data.Json

open Lean

namespace Spec

def providerReply (prompt : String) (script : Json) : Except String String := do
  let prompt := prompt.replace "\r\n" "\n"
  let firstLine := (prompt.splitOn "\n").head!
  let some decision := firstLine.dropPrefix? "You are handling a Sapiens4 decision at node "
    | return "The review is ready."
  let node := (decision.toString.splitOn ".").head!
  let context ← Json.parse (prompt.splitOn "\nContext:\n").getLast!
  let owner ← context.getObjValAs? String "selfId"
  let scripted := (script.getObjValD owner).getObjValD node
  let response := if scripted != .null then scripted else script.getObjValD node
  let response := if response != .null then response else
    json% {event: "FitsSpecialization", mode: "Repl", reply: "The review is ready."}
  return ((response.setObjVal! "reason" "The request fits this review.").setObjVal!
    "evidence" (json% ["The review request"])).compress

end Spec
