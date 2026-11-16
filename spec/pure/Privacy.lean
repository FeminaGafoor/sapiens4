import pure.Observations

namespace Spec

structure Reply where
  status : Nat
  body : String

structure Denial where
  reply : Reply
  before : String
  after : String

def Denial.accepts (observed : Denial) (expected : Nat) (markers : List String) : Bool :=
  observed.reply.status == expected && observed.before == observed.after &&
    markers.all (fun marker => !observed.reply.body.contains marker)

theorem acceptedDenialProtectsEveryMarker {observed : Denial} {expected : Nat} {markers : List String}
    (passed : observed.accepts expected markers = true) :
    observed.reply.status = expected ∧ observed.before = observed.after ∧
    ∀ marker ∈ markers, observed.reply.body.contains marker = false := by
  simpa [Denial.accepts, and_assoc, List.all_eq_true] using passed

def verifyDenial (boundary : String) (expected : Nat) (reply : Reply)
    (before after : Lean.Json) (markers : List String) : Verdict :=
  requireThat s!"{boundary}: expected HTTP {expected}, unchanged protected state and a private response (HTTP {reply.status})"
    ((Denial.mk reply before.compress after.compress).accepts expected markers)

inductive RequestOrigin where
  | absent | local | foreignSite | opaque
  deriving DecidableEq, Repr

structure LocalWrite where
  origin : RequestOrigin
  localHost : Bool
  localIntent : Bool
  jsonBody : Bool
  deriving DecidableEq, Repr

def LocalWrite.allowed (write : LocalWrite) : Bool :=
  (write.origin == .absent || write.origin == .local) && write.localHost && write.localIntent && write.jsonBody

def LocalWrite.cases : List LocalWrite :=
  let origins : List RequestOrigin := [.absent, .local, .foreignSite, .opaque]
  let choices := [false, true]
  origins.flatMap fun origin =>
    choices.flatMap fun localHost =>
      choices.flatMap fun localIntent =>
        choices.map fun jsonBody => ⟨origin, localHost, localIntent, jsonBody⟩

theorem everyLocalWriteIsExercised (write : LocalWrite) : write ∈ LocalWrite.cases := by
  obtain ⟨origin, host, intent, body⟩ := write
  cases origin <;> cases host <;> cases intent <;> cases body <;> decide

theorem onlyTwoLocalWritesAreAllowed : LocalWrite.cases.filter LocalWrite.allowed =
    [⟨.absent, true, true, true⟩, ⟨.local, true, true, true⟩] := by decide

def LocalWrite.headers (write : LocalWrite) (url : String) : Array String := Id.run do
  let origin := match write.origin with
    | .absent => #[]
    | .local => #["Origin: " ++ url]
    | .foreignSite => #["Origin: https://other.invalid"]
    | .opaque => #["Origin: null"]
  let host := if write.localHost then #[] else #["Host: other.invalid"]
  let intent := if write.localIntent then #["X-Sapiens-Local: 1"] else #[]
  return origin ++ host ++ intent ++ #[if write.jsonBody then "Content-Type: application/json" else "Content-Type: text/plain"]

end Spec
