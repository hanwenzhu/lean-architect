import Architect.Load
import Cli


open Lean

/-! This file contains utilities for porting from an existing LaTeX blueprint. -/

open Lean Cli Architect

def runAddPositionInfo (p : Parsed) : IO UInt32 := do
  let some imports := p.flag? "imports" |>.bind (·.as? (Array ModuleName))
    | IO.throwServerError "--imports flag is required"
  let stdin ← IO.getStdin
  let input ← stdin.readToEnd
  let .arr jsons ← IO.ofExcept (Json.parse input)
    | IO.throwServerError "Expected a JSON array"
  let nodes : Array Node ← jsons.filterMapM fun json => do
    match fromJson? json with
    | .ok (node : Node) => return some node
    | .error e =>
      IO.eprintln s!"Ignoring node with error: {e}"
      return none
  runEnvOfImports imports do
    let nodesWithPos ← nodes.mapM fun node => node.toNodeWithPos
    IO.println (nodesWithPos.map NodeWithPos.toJson |>.toJson)
  return 0

def addPositionInfoCmd : Cmd := `[Cli|
  add_position_info VIA runAddPositionInfo;
  "Add position information to a JSON list of nodes."

  FLAGS:
    imports : Array ModuleName; "Comma-separated Lean modules to import."
]

def main (args : List String) : IO UInt32 := do
  addPositionInfoCmd.validate args
