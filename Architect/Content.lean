import Architect.Basic


open Lean Elab

namespace Architect

/-! The blueprint content in a module (see `BlueprintContent`) consists of:

- Blueprint nodes extracted  from `@[blueprint]` tags
- All module docstrings defined in `/-! ... -/`

These contents are sorted by declaration range (similar to the sort in doc-gen4).
-/

deriving instance Repr for ModuleDoc in
/-- The export blueprint LaTeX from a module is determined by the list of `BlueprintContent`
in the module. This is analogous to doc-gen4's `ModuleMember`. -/
inductive BlueprintContent where
  | node : NodeWithPos → BlueprintContent
  | modDoc : ModuleDoc → BlueprintContent
deriving Inhabited, Repr

def BlueprintContent.declarationRange : BlueprintContent → Option DeclarationRange
  | .node n => n.location.map (·.range)
  | .modDoc doc => some doc.declarationRange

/-- An order for blueprint contents, based on their declaration range. -/
def BlueprintContent.order (l r : BlueprintContent) : Bool :=
  match l.declarationRange, r.declarationRange with
  | some l, some r => Position.lt l.pos r.pos
  | some _, none => true
  | _, _ => false

/-- Get blueprint contents of the current module (this is for debugging). -/
def getMainModuleBlueprintContents : CoreM (Array BlueprintContent) := do
  let env ← getEnv
  let nodes ← (blueprintExt.getEntries env).toArray.mapM fun (_, node) => BlueprintContent.node <$> node.toNodeWithPos
  let modDocs := (getMainModuleDoc env).toArray.map BlueprintContent.modDoc
  return (nodes ++ modDocs).qsort BlueprintContent.order

/-- Get blueprint contents of an imported module (this is for debugging). -/
def getBlueprintContents (module : Name) : CoreM (Array BlueprintContent) := do
  let env ← getEnv
  let some modIdx := env.getModuleIdx? module | return #[]
  let nodes ← (blueprintExt.getModuleEntries env modIdx).mapM fun (_, node) => BlueprintContent.node <$> node.toNodeWithPos
  let modDocs := (getModuleDoc? env module).getD #[] |>.map BlueprintContent.modDoc
  return (nodes ++ modDocs).qsort BlueprintContent.order

end Architect
