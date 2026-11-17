import pure.Observations

open Lean
namespace Spec

structure Rectangle where
  x : Float
  y : Float
  width : Float
  height : Float

def bounds (element : Json) : Except String Rectangle := do
  let position ← element.getObjVal? "position"
  let size ← element.getObjVal? "size"
  return ⟨← position.getObjValAs? Float "x", ← position.getObjValAs? Float "y",
    ← size.getObjValAs? Float "width", ← size.getObjValAs? Float "height"⟩

def visibleCenter (control : Json) (windows : Array Json) : Except String (Int × Int) := do
  let target ← bounds control
  let x := target.x + target.width / 2
  let y := target.y + target.height / 2
  let windows ← windows.mapM bounds
  requireThat "control outside the fixture window" (target.width > 0 && target.height > 0 &&
    windows.any (fun window => x ≥ window.x && x < window.x + window.width &&
      y ≥ window.y && y < window.y + window.height))
  return (x.floor.toInt64.toInt, y.floor.toInt64.toInt)

def enabledControl (nodes : Array Json) (label : String) : Except String (Option Json) := do
  let candidates := nodes.filter fun node =>
    (node.getObjValAs? Bool "enabled").toOption == some true &&
    ["title", "description", "value"].any (fun key => (stringField node key).toOption == some label)
  match candidates.toList with
  | [] => return none
  | [button] => return some button
  | _ => throw s!"Ambiguous accessible control: {label}"

end Spec
