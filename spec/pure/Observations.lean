import Lean.Data.Json

open Lean
namespace Spec

abbrev Verdict := Except String Unit

def requireThat (description : String) (condition : Bool) : Verdict :=
  unless condition do throw description

def fieldsMatch (actual expected : Json) : Verdict := do
  for (key, value) in (← expected.getObj?).toList do
    let observed ← actual.getObjVal? key
    requireThat s!"unexpected value for {key}" (observed == value)

def stringField (record : Json) (name : String) : Except String String :=
  record.getObjValAs? String name

def uniqueRecord (records : Json) (key : String) (value : Json) : Except String Json := do
  let matching := (← records.getArr?).filter fun record => (record.getObjVal? key).toOption == some value
  match matching.toList with
  | [record] => return record
  | _ => throw s!"expected one matching {key}, found {matching.size}"

end Spec
