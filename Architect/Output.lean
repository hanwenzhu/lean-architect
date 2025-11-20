import Architect.Content
import MD4Lean


open Lean

namespace Architect

section ToLatex

/-!
Here, we convert nodes and docstrings to LaTeX.
The main difficulty is converting Markdown to LaTeX, which we describe below.
-/

abbrev Latex := String

/-!
We convert docstrings of declarations and modules to LaTeX.
Besides the usual Markdown features, we also support citations using square brackets
and references using inline code, by having custom commands and postprocessing steps,
as follows:

1. Using MD4Lean, we parse the markdown.
2. If possible, we convert bracketed citations (e.g. [taylorwiles]) to \cite{taylorwiles} commands.
3. If possible, we convert inline code with a constant (e.g. `abc`) to \ref{abc} commands.
4. We convert the markdown to LaTeX macro definitions as output.

**TODO**: Support doc-gen4 style named citations like [abc][def].

The output provides the following macros:
- `\inputleannode{name}`: Inputs the theorem or definition with Lean name `name`.
- `\inputleanmodule{Module}`: Inputs the entire module (containing nodes and module docstrings) with module name `Module`.s

The long-term goal for lean-architect is to migrate to Verso
and not have to output to LaTeX at all.
Here, this would mean using docstring parsing from Verso instead
(which similarly uses MD4Lean to parse the docstrings),
and it has support for elaborating code blocks.
However, AFAIK it currently does not support citations in docstrings.
-/

register_option blueprint.bracketedCitations : Bool := {
  defValue := true,
  descr := "Whether to register square-bracketed content as citations (e.g. `[taylorwiles]`). \
            You should set this in the `leanOptions` field of the lakefile."
}

register_option blueprint.refCommand : String := {
  defValue := "ref",
  descr := "The LaTeX command to use for references (e.g. `ref` or `cref`). \
            You should set this in the `leanOptions` field of the lakefile."
}

register_option blueprint.citeCommand : String := {
  defValue := "cite",
  descr := "The LaTeX command to use for citations (e.g. `cite` or `citep`). \
            You should set this in the `leanOptions` field of the lakefile."
}

register_option blueprint.resolveInlineCode : Bool := {
  defValue := true,
  descr := "Whether to try to resolve inline code to references (e.g. `abc` to `\\ref{abc}`). \
            You should set this in the `leanOptions` field of the lakefile."
}

variable {m} [Monad m] [MonadOptions m]

/-- Escapes a string in LaTeX for all LaTeX environments (text, math, leancode, links, etc.). -/
def escapeForLatexBasic (s : String) : String :=
  s.replace "#" "\\#" |>.replace "%" "\\%"

/-- Escapes a string in LaTeX for text. -/
def escapeForLatexText (s : String) : String :=
  escapeForLatexBasic s |>.replace "_" "\\_" |>.replace "^" "\\^{}"

/-- Escape `#` and convert brackets `[taylorwiles]` to `\cite{taylorwiles}` commands. -/
partial def postprocessMarkdownText (s : String) : m String := do
  let s := escapeForLatexText s
  if !blueprint.bracketedCitations.get (← getOptions) then
    return s
  else
    let citeCommand := blueprint.citeCommand.get (← getOptions)
    let allBracketed := findAllEnclosed s '[' ']' (fun c => c == '[' || c == '(')  -- do not convert the first bracket in `[a][b]` or in `[a](b)`
    return allBracketed.foldl (init := s) fun s bracketed =>
      s.replace s!"[{bracketed}]" (s!"\\" ++ citeCommand ++ "{" ++ bracketed ++ "}")
where
  findAllEnclosed (s : String) (bracketStart bracketEnd : Char) (notFollowedBy : Char → Bool) (i : String.Pos := 0) (ret : Array String := ∅) : Array String :=
    let lps := ⟨(s.posOfAux bracketStart s.endPos i).byteIdx + 1⟩
    if lps < s.endPos then
      let lpe := s.posOfAux bracketEnd s.endPos lps
      let nextPos : String.Pos := ⟨lpe.byteIdx + 1⟩
      if lpe < s.endPos && (!nextPos.isValid s || !notFollowedBy (s.get nextPos)) then
        let bracketed := Substring.toString ⟨s, lps, lpe⟩
        findAllEnclosed s bracketStart bracketEnd notFollowedBy lpe (ret.push bracketed)
      else
        ret
    else
      ret

def removeNameFirstComponent : Name → Option Name
  | .str .anonymous _ => some .anonymous
  | .num .anonymous _ => some .anonymous
  | .str p s => removeNameFirstComponent p |>.map fun p => .str p s
  | .num p i => removeNameFirstComponent p |>.map fun p => .num p i
  | .anonymous => none

open MD4Lean in
def parseMarkdown (markdown : String) : Option MD4Lean.Document :=
  parse markdown (parserFlags :=
    -- GitHub-flavored Markdown dialect
    MD_DIALECT_GITHUB |||
    -- Support $...$ and $$...$$
    MD_FLAG_LATEXMATHSPANS |||
    -- Disable raw HTML
    MD_FLAG_NOHTML |||
    -- Docstrings can be group indented, which we do not want to parse as code blocks
    MD_FLAG_NOINDENTEDCODEBLOCKS)

/-- Parse and convert markdown to LaTeX using MD4Lean with postprocessing steps as above. -/
partial def markdownToLatex (markdown : String) : m Latex :=
  match parseMarkdown markdown with
  | none => return markdown
  | some doc => documentToLatex doc
where
  documentToLatex (doc : MD4Lean.Document) : m Latex := do
    return "\n\n".intercalate (← doc.blocks.mapM blockToLatex).toList
  blockToLatex (block : MD4Lean.Block) : m Latex := do
    match block with
    | .p texts =>
      return String.join (← texts.mapM textToLatex).toList
    | .ul _tight _mark items =>
      return "\\begin{itemize}" ++ "\n\n".intercalate (← items.mapM itemToLatex).toList ++ "\\end{itemize}"
    | .ol _tight _start _mark items =>
      return "\\begin{enumerate}" ++ "\n\n".intercalate (← items.mapM itemToLatex).toList ++ "\\end{enumerate}"
    | .hr => return "\\midrule"
    | .header level texts =>
      let headerCommand := match level with | 0 | 1 => "chapter" | 2 => "section" | 3 => "subsection" | 4 => "subsubsection" | 5 => "paragraph" | _ => "subparagraph"
      return "\\" ++ headerCommand ++ "{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .code _info _lang _fenceChar content =>
      -- It seems \begin{verbatim} ... \end{verbatim} is not supported in macros,
      -- so we split to lines and convert each line as inline code.
      let content := String.join content.toList
      let lines ← (content.splitOn "\n").mapM fun line => textToLatex (.code #[line])
      return "\n\\\\\n".intercalate lines
    | .html content => return String.join content.toList
    | .blockquote content => return "\\begin{quote}" ++ "\n\n".intercalate (← content.mapM blockToLatex).toList ++ "\\end{quote}"
    | .table _head _body => return "[lean-architect: table not supported yet]"
  textToLatex (text : MD4Lean.Text) : m Latex := do
    match text with
    | .normal content | .br content | .softbr content | .entity content =>
      postprocessMarkdownText content
    | .nullchar => return ""
    | .em texts => return "\\emph{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .strong texts => return "\\textbf{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .u texts => return "\\ul{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .a href _title _isAuto texts => return "\\href{" ++ String.join (href.map attrTextToLatex).toList ++ "}{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .img src _title _alt => return "\\includegraphics{" ++ String.join (src.map attrTextToLatex).toList ++ "}"
    -- \leancode converts inline code to \ref where possible. If not a valid reference, this defaults to \texttt{\detokenize{content}}
    -- (see `latexPreamble`; **TODO**: use \verb or \Verb instead if possible).
    | .code content => return "\\leancode{" ++ String.join (content.toList.map escapeForLatexBasic) ++ "}"
    | .del texts => return "\\st{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
    | .latexMath content => return "$" ++ String.join (content.toList.map escapeForLatexBasic) ++ "$"
    | .latexMathDisplay content =>
      let math := String.join (content.toList.map escapeForLatexBasic)
      -- $$...$$ is translated to $$...$$ in LaTeX
      -- But $$\begin{align}...\end{align}$$ is translated to \begin{align}...\end{align} instead without $$
      -- This is because we want to support environments beyond the basic math environment $$...$$.
      -- This list is from https://github.com/jgm/pandoc/blob/main/src/Text/Pandoc/Writers/LaTeX.hs
      let displayMathEnvs := ["align", "align*", "flalign", "flalign*", "alignat", "alignat*", "dmath", "dmath*", "dgroup", "dgroup*", "darray", "darray*", "gather", "gather*", "multline", "multline*", "split", "subequations", "equation", "equation*", "eqnarray", "displaymath"]
      if displayMathEnvs.any fun env => math.startsWith ("\\begin{" ++ env ++ "}") then
        return math
      else
        return s!"$${math}$$"
    | .wikiLink target texts => return "\\href{" ++ String.join (target.map attrTextToLatex).toList ++ "}{" ++ String.join (← texts.mapM textToLatex).toList ++ "}"
  attrTextToLatex (attrText : MD4Lean.AttrText) : Latex :=
    match attrText with
    | .normal content | .entity content => escapeForLatexBasic content
    | .nullchar => ""
  itemToLatex (item : MD4Lean.Li MD4Lean.Block) : m Latex := do
    match item with
    | .li _isTask _taskChar _taskMarkOffset blocks =>
      return "\\item " ++ "\n\n".intercalate (← blocks.mapM blockToLatex).toList

def moduleToRelPath (module : Name) (ext : String) : System.FilePath :=
  modToFilePath "module" module ext

def libraryToRelPath (library : Name) (ext : String) : System.FilePath :=
  System.mkFilePath ["library", library.toString (escape := false)] |>.addExtension ext

def NodePart.toLatex (part : NodePart) (title : Option String) (additionalContent : String) : m Latex := do
  let mut out := ""
  out := out ++ "\\begin{" ++ part.latexEnv ++ "}"
  if let some title := title then
    out := out ++ s!"[{← markdownToLatex title}]"

  out := out ++ additionalContent
  if part.leanOk then
    out := out ++ "\\leanok{}"
  if !part.uses.isEmpty || !part.usesRaw.isEmpty then
    out := out ++ "\\uses{" ++ ",".intercalate (part.uses.map (·.toString) ++ part.usesRaw).toList ++ "}"

  out := out ++ (← markdownToLatex part.text)

  out := out ++ "\\end{" ++ part.latexEnv ++ "}"
  return out

def NodeWithPos.toLatex (node : NodeWithPos) : m Latex := do
  -- position string as annotation
  let posStr := match node.file, node.location with
    | some file, some location => s!"{file}:{location.range.pos.line}.{location.range.pos.column}-{location.range.endPos.line}.{location.range.endPos.column}"
    | _, _ => ""

  let mut addLatex := ""
  addLatex := addLatex ++ "\\lean{" ++ node.name.toString ++ "}"
  addLatex := addLatex ++ "\\label{" ++ node.name.toString ++ "}"
  if node.notReady then
    addLatex := addLatex ++ "\\notready"
  if let some d := node.discussion then
    addLatex := addLatex ++ "\\discussion{" ++ toString d ++ "}"
  addLatex := addLatex ++ s!"\n% at {posStr}\n"

  let statementLatex ← node.statement.toLatex node.title addLatex
  match node.proof with
  | none => return statementLatex
  | some proof =>
    let proofLatex ← proof.toLatex none ""
    return statementLatex ++ proofLatex

def NodeWithPos.toLatexHeader (node : NodeWithPos) : m Latex := do
  let latex ← node.toLatex
  return "\\newleannode{" ++ node.name.toString ++ "}{" ++ latex ++ "}"

def BlueprintContent.toLatex : BlueprintContent → m Latex
  | .node n => return "\\inputleannode{" ++ n.name.toString ++ "}"
  | .modDoc d => markdownToLatex d.doc

def latexPreamble : m Latex := do
  let refCommand := blueprint.refCommand.get (← getOptions)
  let resolveInlineCode := blueprint.resolveInlineCode.get (← getOptions)
  let leancodeCommand :=
    if resolveInlineCode then
      "\\@ifundefined{leannode@#1}{\\texttt{\\detokenize{#1}}}{\\" ++ refCommand ++ "{#1}}"
    else
      "\\texttt{\\detokenize{#1}}"
  return "%%% THIS FILE IS AUTO-GENERATED BY BLUEPRINT-GEN. %%%

%%% Macro definitions for \\inputleannode, \\inputleanmodule %%%

\\makeatletter

% \\newleannode{name}{latex} defines a new Lean node
\\providecommand{\\newleannode}[2]{%
  \\expandafter\\gdef\\csname leannode@#1\\endcsname{#2}}
% \\inputleannode{name} inputs a Lean node
\\providecommand{\\inputleannode}[1]{%
  \\csname leannode@#1\\endcsname}

% \\leancode{name} is like \\texttt, but resolves to a reference to a Lean node if possible
\\providecommand{\\leancode}[1]{" ++ leancodeCommand ++ "}

% \\newleanmodule{module}{latex} defines a new Lean module
\\providecommand{\\newleanmodule}[2]{%
  \\expandafter\\gdef\\csname leanmodule@#1\\endcsname{#2}}
% \\inputleanmodule{module} inputs a Lean module
\\providecommand{\\inputleanmodule}[1]{%
  \\csname leanmodule@#1\\endcsname}

\\makeatother

%%% Start of main content %%%"

private def moduleToLatexHeaderAux (module : Name) (contents : Array BlueprintContent) : m Latex := do
  let nodeHeaders ← contents.filterMapM fun
    | .node n => some <$> n.toLatexHeader
    | _ => pure none
  let moduleLatex ← contents.mapM BlueprintContent.toLatex
  let preamble ← latexPreamble
  return preamble ++ "\n\n" ++ "\n\n".intercalate nodeHeaders.toList ++ "\n\n" ++
    "\\newleanmodule{" ++ module.toString ++ "}{" ++ "\n\n".intercalate moduleLatex.toList ++ "}"

/-- Convert imported module to LaTeX. -/
def moduleToLatexHeader (module : Name) : CoreM Latex := do
  let contents ← getBlueprintContents module
  moduleToLatexHeaderAux module contents

/-- Convert current module to LaTeX (for debugging). -/
def mainModuleToLatexHeader : CoreM Latex := do
  let contents ← getMainModuleBlueprintContents
  moduleToLatexHeaderAux (← getMainModule) contents

/-- Shows the blueprint LaTeX of the current module for debugging. -/
syntax (name := show_blueprint) "#show_blueprint" (ppSpace ident)? : command

open Elab Command in
@[command_elab show_blueprint] def elabShowBlueprint : CommandElab
  | `(command| #show_blueprint) => do
    let latex ← liftCoreM mainModuleToLatexHeader
    logInfo m!"Exported blueprint LaTeX of current module:\n\n{latex}"
  | `(command| #show_blueprint $mod:ident) => do
    let mod := mod.getId
    if (← getEnv).getModuleIdx? mod |>.isNone then
      throwError "Unknown module {mod}"
    let latex ← liftCoreM <| moduleToLatexHeader mod
    logInfo m!"Exported blueprint LaTeX of module {mod}:\n\n{latex}"
  | _ => throwUnsupportedSyntax

end ToLatex

section ToJson

private def rangeToJson (range : DeclarationRange) : Json :=
  json% {
    "pos": $(range.pos),
    "endPos": $(range.endPos)
  }

private def locationToJson (location : DeclarationLocation) : Json :=
  json% {
    "module": $(location.module),
    "range": $(rangeToJson location.range)
  }

def NodeWithPos.toJson (node : NodeWithPos) : Json :=
  json% {
    "name": $(node.name),
    "statement": $(node.statement),
    "proof": $(node.proof),
    "notReady": $(node.notReady),
    "discussion": $(node.discussion),
    "title": $(node.title),
    "hasLean": $(node.hasLean),
    "file": $(node.file),
    "location": $(node.location.map locationToJson)
  }

def BlueprintContent.toJson : BlueprintContent → Json
  | .node n => json% {"type": "node", "data": $(n.toJson)}
  | .modDoc d => json% {"type": "moduleDoc", "data": $(d.doc)}

def moduleToJson (module : Name) : CoreM Json := do
  return Json.arr <|
    (← getBlueprintContents module).map BlueprintContent.toJson

def mainModuleToJson : CoreM Json := do
  return Json.arr <|
    (← getMainModuleBlueprintContents).map BlueprintContent.toJson

/-- Shows the blueprint JSON of the current module for debugging. -/
syntax (name := show_blueprint_json) "#show_blueprint_json" (ppSpace ident)? : command

open Elab Command in
@[command_elab show_blueprint_json] def elabShowBlueprintJson : CommandElab
  | `(command| #show_blueprint_json) => do
    let json ← liftCoreM mainModuleToJson
    logInfo m!"Exported blueprint JSON of current module:\n\n{json}"
  | `(command| #show_blueprint_json $mod:ident) => do
    let mod := mod.getId
    if (← getEnv).getModuleIdx? mod |>.isNone then
      throwError "Unknown module {mod}"
    let json ← liftCoreM <| moduleToJson mod
    logInfo m!"Exported blueprint JSON of module {mod}:\n\n{json}"
  | _ => throwUnsupportedSyntax

end ToJson

open IO

/-- Write the result `content` to the appropriate blueprint file with extension `ext` ("tex" or "json"). -/
def outputResultsWithExt (basePath : System.FilePath) (module : Name) (content : String) (ext : String) : IO Unit := do
  FS.createDirAll basePath
  let filePath := basePath / moduleToRelPath module ext
  if let some d := filePath.parent then
    FS.createDirAll d
  FS.writeFile filePath content

/-- Write `latex` to the appropriate blueprint tex file. -/
def outputLatexResults (basePath : System.FilePath) (module : Name) (latex : Latex) : IO Unit := do
  let content := latex
  outputResultsWithExt basePath module content "tex"

/-- Write `json` to the appropriate blueprint json file. -/
def outputJsonResults (basePath : System.FilePath) (module : Name) (json : Json) : IO Unit := do
  let content := json.pretty
  outputResultsWithExt basePath module content "json"

/-- Write to an appropriate index tex file that \inputs all modules in a library. -/
def outputLibraryLatex (basePath : System.FilePath) (library : Name) (modules : Array Name) : IO Unit := do
  FS.createDirAll basePath
  let latex : Latex := "\n\n".intercalate
    (modules.map fun mod => "\\input{" ++ (basePath / moduleToRelPath mod "tex").toString ++ "}").toList
  let filePath := basePath / libraryToRelPath library "tex"
  if let some d := filePath.parent then
    FS.createDirAll d
  FS.writeFile filePath latex

/-- Write to an appropriate index json file containing paths to json files of all modules in a library. -/
def outputLibraryJson (basePath : System.FilePath) (library : Name) (modules : Array Name) : IO Unit := do
  FS.createDirAll basePath
  let json : Json := Json.mkObj [("modules", toJson (modules.map fun mod => moduleToRelPath mod "json"))]
  let content := json.pretty
  let filePath := basePath / libraryToRelPath library "json"
  if let some d := filePath.parent then
    FS.createDirAll d
  FS.writeFile filePath content

end Architect
