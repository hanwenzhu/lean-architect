import Lean
import Batteries.Lean.NameMapAttribute
import Architect.Basic


open Lean Elab Tactic Meta

namespace Architect

namespace ProofDocString

/-! Here we implement docstrings but for proofs. -/

-- NB: I copied some logic from `aliasExtension`

abbrev State := SMap Name (Array String)
abbrev Entry := Name × String

private def addEntryFn (s : State) (e : Entry) : State :=
  match s.find? e.1 with
  | none => s.insert e.1 #[e.2]
  | some es => s.insert e.1 (es.push e.2)

initialize proofDocStringExt : SimplePersistentEnvExtension Entry State ←
  registerSimplePersistentEnvExtension {
    addEntryFn := addEntryFn
    addImportedFn := fun es => mkStateFromImportedEntries addEntryFn {} es |>.switch
    asyncMode := .async .asyncEnv
  }

end ProofDocString

open ProofDocString

def addProofDocString (env : Environment) (name : Name) (doc : String) : Environment :=
  proofDocStringExt.addEntry (asyncDecl := name) env (name, doc)

def getProofDocString (env : Environment) (name : Name) : Array String :=
  proofDocStringExt.getState (asyncDecl := name) env |>.findD name #[]

elab (name := tacticDocComment) docComment:docComment t:tactic : tactic => do
  let some name ← Term.getDeclName? | throwError "could not get declaration name"
  validateDocComment docComment
  let doc := (← getDocStringText docComment).trim
  modifyEnv fun env => addProofDocString env name doc
  -- NOTE: an alternative approach is to remove `t:tactic` and `evalTactic t`.
  -- This would also work for our purpose, but we require a following `t:tactic` and then immediately
  -- evaluate it because this would avoid the unusedTactic linter in Mathlib to flag the docComment
  -- (and we do not currently import Mathlib and hence cannot modify to ignore `tacticDocComment`).
  evalTactic t

/-! We implement the `blueprint_using` and `sorry_using` tactics that declares used constants. -/

-- **TODO**: support `sorry_using ["label"]` (which should elaborate to `let _ : RawLabel := "label"` where `RawLabel := String`,
-- and then collect all terms of type `RawLabel` to the blueprint metadata in `Attribute.lean`).

/--
`blueprint_using [a, b]` adds `a` and `b` as dependencies for the blueprint metadata.

It is basically the same as `let := a; let := b`.
-/
elab "blueprint_using" " [" ids:ident,* "]" : tactic => do
  for id in ids.getElems do
    let used ← realizeGlobalConstNoOverloadWithInfo id
    let info ← getConstInfo used
    -- Instantiate universe level parameters with 0, to avoid errors
    let lvls := List.replicate info.numLevelParams levelZero
    let ty := info.instantiateTypeLevelParams lvls
    let const := mkConst used lvls
    liftMetaTactic1 fun g => do
      let g' ← g.define (← mkFreshBinderNameForTactic `blueprint_using) ty const
      let (_, g'') ← g'.intro1P
      return g''

/--
`sorry_using [a, b]` is the same as `sorry`, but adds `a` and `b` as dependencies for the blueprint metadata.

It is basically similar to `let := a; let := b; sorry`.
-/
elab (name := tacticSorryUsing) "sorry_using" " [" ids:ident,* "]" : tactic => do
  evalTactic (← `(tactic| blueprint_using [$[$ids],*]))
  liftMetaTactic1 fun g => do
    let mut g := g
    -- We touch every local hypothesis to avoid unused variable linter
    -- This is not an elegant solution, but it works
    for h in ← getLocalHyps do
      g ← g.define (← mkFreshBinderNameForTactic `sorry_using) (← inferType h) h
      (_, g) ← g.intro1P
    -- A non-synthetic sorry because the `sorry_using` tactic is explicitly written by the user
    g.admit (synthetic := false)
    return g

@[inherit_doc tacticSorryUsing]
macro (name := termSorryUsing) "sorry_using" " [" ids:ident,* "]" : term =>
  `(term| by sorry_using [$[$ids],*])

end Architect
