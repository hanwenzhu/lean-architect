import Lean
import Architect.CollectUsed
import Architect.Content
import Architect.Tactic


open Lean Meta Elab

namespace Architect

/-- `Config` is the type of arguments that can be provided to `blueprint`. -/
structure Config where
  /-- The statement of the node in text. Uses docstring if not present. -/
  statement : Option String := none
  /-- By default, only theorems have separate proof parts. This option overrides this behavior. -/
  hasProof : Option Bool := none
  /-- The proof of the node in text. Uses proof docstrings if not present. -/
  proof : Option String := none
  /-- The set of nodes that this node depends on. Infers from the constant if not present. -/
  uses : Array Name := #[]
  /-- Additional raw labels of nodes that this node depends on. -/
  usesRaw : Array String := #[]
  /-- The set of nodes that the proof of this node depends on. Infers from the constant's value if not present. -/
  proofUses : Array Name := #[]
  /-- Additional raw labels of nodes that the proof of this node depends on. -/
  proofUsesRaw : Array String := #[]
  /-- The surrounding environment is not ready to be formalized, typically because it requires more blueprint work. -/
  notReady : Bool := false
  /-- A GitHub issue number where the surrounding definition or statement is discussed. -/
  discussion : Option Nat := none
  /-- The short title of the node in LaTeX. -/
  title : Option String := none
  /-- The LaTeX environment to use for the node. -/
  latexEnv : Option String := none
  /-- Enable debugging. -/
  trace : Bool := false
deriving Repr

syntax blueprintStatementOption := &"statement" " := " docComment
syntax blueprintHasProofOption := &"hasProof" " := " ("true" <|> "false")
syntax blueprintProofOption := &"proof" " := " docComment
syntax blueprintUsesOption := &"uses" " := " "[" (ident <|> str),* "]"
syntax blueprintProofUsesOption := &"proofUses" " := " "[" (ident <|> str),* "]"
syntax blueprintNotReadyOption := &"notReady" " := " ("true" <|> "false")
syntax blueprintDiscussionOption := &"discussion" " := " num
syntax blueprintLatexEnvOption := &"latexEnv" " := " str

syntax blueprintOption := "("
  blueprintStatementOption <|>
  blueprintHasProofOption <|> blueprintProofOption <|>
  blueprintUsesOption <|> blueprintProofUsesOption <|>
  blueprintNotReadyOption <|> blueprintDiscussionOption <|>
  blueprintLatexEnvOption ")"
syntax blueprintOptions := (ppSpace str)? (ppSpace blueprintOption)*

/--
The `blueprint` attribute tags a constant to add to the blueprint.

You may optionally add:
- `"Title"`: The title of the node in LaTeX.
- `statement := /-- ... -/`: The statement of the node in text (default: the docstring).
- `hasProof := true`: If the node has a proof (default: true if the node is a theorem).
- `proof := /-- ... -/`: The proof of the node in text (default: the docstrings in proof tactics).
- `uses := [a, b]`: The dependencies of the node (default: inferred from the used constants).
- `proofUses := [a, b]`: The dependencies of the proof of the node (default: inferred from the used constants).
- `notReady := true`: Whether the node is not ready.
- `discussion := 123`: The discussion issue number of the node.
- `latexEnv := "lemma"`: The LaTeX environment to use for the node (default: "theorem" or "definition").

For more information, see [lean-architect](https://github.com/hanwenzhu/lean-architect).

Use `blueprint?` to show the raw data of the added node.
-/
syntax (name := blueprint) "blueprint" "?"? blueprintOptions : attr

@[inherit_doc blueprint]
macro "blueprint?" opts:blueprintOptions : attr => `(attr| blueprint ? $opts)

/-- Elaborates the configuration options for `blueprint`. -/
def elabBlueprintConfig : Syntax → CoreM Config
  | `(attr| blueprint%$_tk $[?%$trace?]? $[$title?:str]? $[$opts:blueprintOption]*) => do
    let mut config : Config := { trace := trace?.isSome }
    if let some title := title? then config := { config with title := title.getString }
    for stx in opts do
      match stx with
      | `(blueprintOption| (statement := $doc)) =>
        validateDocComment doc
        let statement := (← getDocStringText doc).trim
        config := { config with statement }
      | `(blueprintOption| (hasProof := true)) =>
        config := { config with hasProof := some .true }
      | `(blueprintOption| (hasProof := false)) =>
        config := { config with hasProof := some .false }
      | `(blueprintOption| (proof := $doc)) =>
        validateDocComment doc
        let proof := (← getDocStringText doc).trim
        config := { config with proof }
      | `(blueprintOption| (uses := [$[$ids],*])) =>
        let uses ← ids.filterMapM fun
          | `(ident| $id:ident) => some <$> tryResolveConst id
          | _ => pure none
        let usesRaw := ids.filterMap fun
          | `(str| $str:str) => some str.getString
          | _ => none
        config := { config with uses := config.uses ++ uses, usesRaw := config.usesRaw ++ usesRaw }
      | `(blueprintOption| (proofUses := [$[$ids],*])) =>
        let proofUses ← ids.filterMapM fun
          | `(ident| $id:ident) => some <$> tryResolveConst id
          | _ => pure none
        let proofUsesRaw := ids.filterMap fun
          | `(str| $str:str) => some str.getString
          | _ => none
        config := { config with proofUses := config.proofUses ++ proofUses, proofUsesRaw := config.proofUsesRaw ++ proofUsesRaw }
      | `(blueprintOption| (notReady := true)) =>
        config := { config with notReady := .true }
      | `(blueprintOption| (notReady := false)) =>
        config := { config with notReady := .false }
      | `(blueprintOption| (discussion := $n)) =>
        config := { config with discussion := n.getNat }
      | `(blueprintOption| (latexEnv := $str)) =>
        config := { config with latexEnv := str.getString }
      | _ => throwUnsupportedSyntax
    return config
  | _ => throwUnsupportedSyntax

/-- Whether a node has a proof part. -/
def hasProof (name : Name) (cfg : Config) : CoreM Bool := do
  return cfg.hasProof.getD (cfg.proof.isSome || wasOriginallyTheorem (← getEnv) name)

def mkStatementPart (name : Name) (cfg : Config) (hasProof : Bool) (used : NameSet) :
    CoreM NodePart := do
  let env ← getEnv
  let leanOk := !used.contains ``sorryAx
  -- Used constants = constants specified by `uses :=` + blueprint constants used in the statement
  let uses := used.filter fun c => (blueprintExt.find? env c).isSome
  let uses := cfg.uses.foldl (·.insert ·) uses
  -- Use docstring for statement text
  let statement := cfg.statement.getD ((← findSimpleDocString? env name).getD "").trim
  return {
    leanOk
    text := statement
    uses := uses.toArray
    usesRaw := cfg.usesRaw
    latexEnv := cfg.latexEnv.getD (if hasProof then "theorem" else "definition")
  }

def mkProofPart (name : Name) (cfg : Config) (used : NameSet) : CoreM NodePart := do
  let env ← getEnv
  let leanOk := !used.contains ``sorryAx
  -- Used constants = constants specified by `proofUses :=` + blueprint constants used in the proof
  let uses := used.filter fun c => (blueprintExt.find? env c).isSome
  let uses := cfg.proofUses.foldl (·.insert ·) uses
  -- Use proof docstring for proof text
  let proof := cfg.proof.getD ("\n\n".intercalate (getProofDocString env name).toList)
  return {
    leanOk
    text := proof
    uses := uses.toArray
    usesRaw := cfg.proofUsesRaw
    latexEnv := "proof"
  }

def mkNode (name : Name) (cfg : Config) : CoreM Node := do
  let (statementUsed, proofUsed) ← collectUsed name
  if ← hasProof name cfg then
    let statement ← mkStatementPart name cfg .true statementUsed
    let proof ← mkProofPart name cfg proofUsed
    return { cfg with name, statement, proof }
  else
    let used := statementUsed ∪ proofUsed
    let statement ← mkStatementPart name cfg .false used
    return { cfg with name, statement, proof := none }

/--
Raises an error if `newName` occurs in the (irreflexive transitive) dependencies of `node`.
If ignored, this would create a cycle and then an error during `leanblueprint web`.

(Note: this check will only raise an error if `blueprint.ignoreUnknownConstants` is true,
which may permit cyclic dependencies.)
-/
partial def checkCyclicUses (newName : Name) (node : Node) (visited : NameSet := ∅) (path : Array Name := #[]) : CoreM Unit := do
  let path' := path.push node.name
  if visited.contains node.name then
    if path.contains node.name then
      throwError "cyclic dependency in blueprint:\n  {" uses ".intercalate (path'.toList.map toString)}"
    else
      return
  let visited' := visited.insert node.name
  for use in node.statement.uses do
    if let some used := blueprintExt.find? (← getEnv) use then
      checkCyclicUses newName used visited' path'
  if let some proof := node.proof then
    for use in proof.uses do
      if let some used := blueprintExt.find? (← getEnv) use then
        checkCyclicUses newName used visited' path'

initialize registerBuiltinAttribute {
    name := `blueprint
    descr := "Adds a node to the blueprint"
    applicationTime := .afterCompilation
    add := fun name stx kind => do
      unless kind == AttributeKind.global do throwError "invalid attribute 'blueprint', must be global"
      let cfg ← elabBlueprintConfig stx
      withOptions (·.updateBool `trace.blueprint (cfg.trace || ·)) do

      let node ← mkNode name cfg
      blueprintExt.add name node
      trace[blueprint] "Blueprint node added:\n{repr node}"

      checkCyclicUses name node

      -- pushInfoLeaf <| .ofTermInfo {
      --   elaborator := .anonymous, lctx := {}, expectedType? := none,
      --   stx, expr := mkStrLit (repr node).pretty }
  }

end Architect
