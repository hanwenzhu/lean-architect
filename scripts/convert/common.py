import subprocess
import re
import json
import sys
from typing import Optional

from loguru import logger

from pydantic import BaseModel, ConfigDict
from pydantic.alias_generators import to_camel


def _quote(s: str) -> str:
    """Quotes a string in double quotes."""
    return json.dumps(s, ensure_ascii=False)


class BaseSchema(BaseModel):
    """A Pydantic base model with camelCase aliases."""
    model_config = ConfigDict(
        alias_generator=to_camel,
        populate_by_name=True,
    )

# These classes are ported from Architect/Basic.lean

class NodePart(BaseSchema):
    lean_ok: bool
    text: str
    uses: set[str]
    uses_raw: set[str]
    latex_env: str

    def all_uses(self) -> list[str]:
        return [use for use in self.uses] + [_quote(use) for use in self.uses_raw]

def make_docstring(text: str, indent: int = 0) -> str:
    text = text.strip()
    text = text.replace("\n", f"\n{' ' * indent}")
    if "\n" in text:
        return f"/--\n{' ' * indent}{text}\n{' ' * indent}-/"
    else:
        return f"/-- {text} -/"

class Node(BaseSchema):
    name: str  # Lean identifier (unique)
    statement: NodePart
    proof: Optional[NodePart]
    not_ready: bool
    discussion: Optional[int]
    title: Optional[str]

    @property
    def uses(self) -> set[str]:
        return self.statement.uses | (self.proof.uses if self.proof is not None else set())

    def to_lean_attribute(
        self,
        add_statement_text: bool = True, add_uses: bool = True, add_uses_raw: bool = True,
        add_proof_text: bool = True, add_proof_uses: bool = True, add_proof_uses_raw: bool = True
    ) -> str:
        configs = []
        # See Architect/Attribute.lean for the options
        if self.title:
            configs.append(_quote(self.title))
        if add_statement_text and self.statement.text.strip():
            configs.append(f"(statement := {make_docstring(self.statement.text, indent=2)})")
        if add_uses and self.statement.uses:
            configs.append(f"(uses := [{', '.join(self.statement.uses)}])")
        if add_uses_raw and self.statement.uses_raw:
            configs.append(f"(uses := [{', '.join(_quote(use) for use in self.statement.uses_raw)}])")
        if self.proof is not None:
            if add_proof_text and self.proof.text.strip():
                configs.append(f"(proof := {make_docstring(self.proof.text, indent=2)})")
            if add_proof_uses and self.proof.uses:
                configs.append(f"(proofUses := [{', '.join(self.proof.uses)}])")
            if add_proof_uses_raw and self.proof.uses_raw:
                configs.append(f"(proofUses := [{', '.join(_quote(use) for use in self.proof.uses_raw)}])")
        if self.not_ready:
            configs.append("(notReady := true)")
        if self.discussion:
            configs.append(f"(discussion := {self.discussion})")
        if self.proof is None and self.statement.latex_env != "definition" or self.proof is not None and self.statement.latex_env != "theorem":
            configs.append(f"(latexEnv := {_quote(self.statement.latex_env)})")
        config = "".join(f"\n  {config}" for config in configs)
        return f"blueprint{config}"

class Position(BaseSchema):
    line: int
    column: int

class DeclarationRange(BaseSchema):
    pos: Position
    end_pos: Position

class DeclarationLocation(BaseSchema):
    module: str
    range: DeclarationRange

class NodeWithPos(Node):
    has_lean: bool
    location: Optional[DeclarationLocation]
    file: Optional[str]


PANDOC_DEFAULT_WIDTH = 100

def pandoc_convert(from_format: str, to_format: str, input: str) -> str:
    result = subprocess.run(
        [
            "pandoc", "-f", from_format, "-t", to_format,
            f"--columns={PANDOC_DEFAULT_WIDTH}"
        ],
        check=True,
        input=input,
        stdout=subprocess.PIPE,
        stderr=sys.stderr,
        text=True,
    )
    return result.stdout

def pandoc_convert_latex_to_markdown(latex: str) -> str:
    # Preprocess all citation commands to \cite
    # From https://github.com/jgm/pandoc/blob/main/src/Text/Pandoc/Readers/LaTeX/Citation.hs
    cite_commands = ["cite", "Cite", "citep", "citep*", "citeal", "citealp", "citealp*", "autocite", "smartcite", "footcite", "parencite", "supercite", "footcitetext", "citeyearpar", "citeyear", "autocite*", "cite*", "parencite*", "textcite", "citet", "citet*", "citealt", "citealt*", "textcites", "cites", "autocites", "footcites", "parencites", "supercites", "footcitetexts", "Autocite", "Smartcite", "Footcite", "Parencite", "Supercite", "Footcitetext", "Citeyearpar", "Citeyear", "Autocite*", "Cite*", "Parencite*", "Textcite", "Textcites", "Cites", "Autocites", "Footcites", "Parencites", "Supercites", "Footcitetexts", "citetext", "citeauthor", "nocite"]
    latex = re.sub(
        r"\\(?:" + "|".join(c.replace("*", r"\*") for c in cite_commands) + r")\s*(\[.*?\])?\s*\{(.*?)\}",
        r"\\cite\1{\2}",
        latex
    )

    # Call Pandoc to convert LaTeX to Markdown
    converted = pandoc_convert(
        "latex",
        # Pandoc's Markdown flavor, disable raw HTML, disable attributes
        "markdown-raw_html-raw_attribute-bracketed_spans-native_divs-native_spans-link_attributes",
        latex
    )

    # Fix for pandoc bug: https://github.com/jgm/pandoc/issues/11257
    # Remove paragraph breaks before \end.
    converted = re.sub(r"\s*\n\s*\\end\s*\{(.*?)\}", r"\n\\end{\1}", converted)

    # Postprocess outputs of \ref commands
    # Here, the \ref commands that refer to depgraph nodes were already replaced with \verb in parse_latex.py
    # Pandoc converts the rest (e.g. \ref{chapter-label}) to [\[chapter-label\]](#chapter-label), which we convert back to \ref{chapter-label}
    converted = re.sub(r"\[\\\[(.*?)\\\]\]\(\#\1\)", r"\\ref{\1}", converted)
    # Postprocess citations: [@a; @b text] -> [a] [b], text
    def replace_cite(match):
        parts = match.group(1).split(";")
        tags = " ".join(f"[{p.strip().removeprefix('@')}]" for p in parts)
        rest = match.group(2).strip()
        if rest:
            return f"{tags}, {rest}"
        else:
            return tags
    converted = re.sub(r"\[((?:@[^\s;]+)(?:;\s*@[^\s;]+)*)(.*?)\]", replace_cite, converted)
    return converted.strip()

def convert_node_latex_to_markdown(node: Node):
    node.statement.text = pandoc_convert_latex_to_markdown(node.statement.text)
    if node.proof is not None:
        node.proof.text = pandoc_convert_latex_to_markdown(node.proof.text)
