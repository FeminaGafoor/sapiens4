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

end Spec
