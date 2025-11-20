import Lean
import Architect.Basic

/-!
This is similar to Lean's `collectAxioms`, but collects nodes in the blueprint (plus all axioms)
rather than just axioms.

TODO: maybe run this at LaTeX output time rather than during @[blueprint] tagging
-/

namespace Architect

open Lean

namespace CollectUsed

structure State where
  visited : NameSet    := {}
  used    : Array Name := #[]

abbrev M := ReaderT Environment $ StateM State

partial def collect (c : Name) : M Unit := do
  let collectExpr (e : Expr) : M Unit := e.getUsedConstants.forM collect
  let s ← get
  unless s.visited.contains c do
    modify fun s => { s with visited := s.visited.insert c }
    let env ← read
    if (blueprintExt.find? env c).isSome then
      modify fun s => { s with used := s.used.push c }
    else
      -- This line is `match env.checked.get.find? c with` in Lean.CollectAxioms
      match env.find? c with
      | some (ConstantInfo.axiomInfo _)  => modify fun s => { s with used := s.used.push c }
      | some (ConstantInfo.defnInfo v)   => collectExpr v.type *> collectExpr v.value
      | some (ConstantInfo.thmInfo v)    => collectExpr v.type *> collectExpr v.value
      | some (ConstantInfo.opaqueInfo v) => collectExpr v.type *> collectExpr v.value
      | some (ConstantInfo.quotInfo _)   => pure ()
      | some (ConstantInfo.ctorInfo v)   => collectExpr v.type
      | some (ConstantInfo.recInfo v)    => collectExpr v.type
      | some (ConstantInfo.inductInfo v) => collectExpr v.type *> v.ctors.forM collect
      | none                             => pure ()

end CollectUsed

/--
Returns the transitive set of blueprint nodes that a constant depends on,
as a pair of sets (constants used by type, constants used by value).
They are made disjoint except that possibly both contain `sorryAx`.
-/
def collectUsed [Monad m] [MonadEnv m] [MonadError m] (constName : Name) :
    m (NameSet × NameSet) := do
  let env ← getEnv
  let mut s : CollectUsed.State := {}

  -- Collect constants used by statement
  let info ← getConstInfo constName
  for c in info.type.getUsedConstants do
    (_, s) := ((CollectUsed.collect c).run env).run s
  let typeUsed := NameSet.ofArray s.used

  -- Collect constants used by proof
  (_, s) := ((CollectUsed.collect constName).run env).run s
  let valueUsed := NameSet.ofArray s.used

  return (typeUsed, valueUsed \ typeUsed.erase ``sorryAx)

end Architect
