import Lean
import Batteries.Lean.NameMapAttribute


open Lean Elab

namespace Architect

initialize registerTraceClass `blueprint

/-- The statement or proof of a node. -/
structure NodePart where
  /-- Whether the part is formalized without `sorry` in Lean. -/
  leanOk : Bool
  /-- The natural language description of this part. -/
  text : String
  /-- The names of nodes that this node depends on. -/
  uses : Array Name
  /-- Additional raw labels of nodes that this node depends on, specified by `uses := ["label"]`. -/
  usesRaw : Array String := #[]
  /-- The LaTeX environment to use for this part. -/
  latexEnv : String
deriving Inhabited, Repr, FromJson, ToJson, ToExpr

/-- A theorem or definition in the blueprint graph. -/
structure Node where
  /-- The Lean name of the tagged constant. -/
  name : Name
  /-- The statement of this node. -/
  statement : NodePart
  /-- The proof of this node. -/
  proof : Option NodePart
  /-- The surrounding environment is not ready to be formalized, typically because it requires more blueprint work. -/
  notReady : Bool
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat
  /-- The short title of the node in LaTeX. -/
  title : Option String
deriving Inhabited, Repr, FromJson, ToJson, ToExpr

structure NodeWithPos extends Node where
  /--
  Whether the node name is in the environment.
  This should always be true for nodes e.g. added by `@[blueprint]`.
  -/
  hasLean : Bool
  /-- The location (module & range) the node is defined in. -/
  location : Option DeclarationLocation
  /-- The file the node is defined in. -/
  file : Option System.FilePath
deriving Inhabited, Repr

def Node.toNodeWithPos (node : Node) : CoreM NodeWithPos := do
  let env ← getEnv
  if !env.contains node.name then
    return { node with hasLean := false, location := none, file := none }
  let module := match env.getModuleIdxFor? node.name with
    | some modIdx => env.allImportedModuleNames[modIdx]!
    | none => env.header.mainModule
  let location := match ← findDeclarationRanges? node.name with
    | some ranges => some { module, range := ranges.range }
    | none => none
  let file ← (← getSrcSearchPath).findWithExt "lean" module
  return { node with hasLean := true, location, file }

/-- Environment extension that stores the nodes of the blueprint. -/
initialize blueprintExt : NameMapExtension Node ← registerNameMapExtension _

section ResolveConst

register_option blueprint.ignoreUnknownConstants : Bool := {
  defValue := .false,
  descr := "Whether to ignore unknown constants in the `uses` and `proofUses` options of the `blueprint` attribute."
}

/--
Resolves an identifier using `realizeGlobalConstNoOverloadWithInfo`.
Ignores unknown constants if `blueprint.ignoreUnknownConstants` is true (default: false).
-/
def tryResolveConst (id : Ident) : CoreM Name := do
  try
    realizeGlobalConstNoOverloadWithInfo id
  catch ex =>
    if blueprint.ignoreUnknownConstants.get (← getOptions) then
      -- logNamedWarningAt id lean.unknownIdentifier ex.toMessageData
      return id.getId
    else
      throwNamedErrorAt id lean.unknownIdentifier
        "{ex.toMessageData}\n\nThis error can be disabled by `set_option blueprint.ignoreUnknownConstants true`."

end ResolveConst

end Architect
