"""Utilities for adding @[blueprint] attributes to Lean source files."""

from pathlib import Path
import re
from typing import Optional

from loguru import logger

from common import Node, NodeWithPos, Position, DeclarationRange, DeclarationLocation, make_docstring


def split_declaration(source: str, pos: Position, end_pos: Position):
    """Split a Lean file into pre, declaration, and post parts."""
    lines = source.splitlines(keepends=True)

    # -1 because Lean Position is 1-indexed
    start = sum(len(lines[i]) for i in range(pos.line - 1)) + pos.column
    end = sum(len(lines[i]) for i in range(end_pos.line - 1)) + end_pos.column

    pre = source[:start]
    decl = source[start:end]
    post = source[end:]

    return pre, decl, post


warned_to_additive = False

def insert_docstring_and_attribute(decl: str, new_docstring: str, new_attr: str) -> str:
    """Inserts attribute and docstring to the declaration.

    Note: This function assumes that the declaration is written in a "parseable" style,
    and corner cases would be fixed manually.
    """

    # open ... in, omit ... in, include ... in, etc (assuming one-line, ending in newline, no interfering comments, etc)
    match = re.search(r"^(?:[a-zA-Z_]+.*?in\n)+", decl)
    if match:
        command_modifiers = match.group(0)
        decl = decl.removeprefix(match.group(0))
    else:
        command_modifiers = ""

    match = re.search(r"^\s*/--(.*?)-/\s*", decl, flags=re.DOTALL)
    if match:
        docstring = f"{new_docstring}\n\n{match.group(1).strip()}"
        decl = decl.removeprefix(match.group(0))
    else:
        docstring = new_docstring

    match = re.search(r"^\s*@\[(.*?)\]\s*", decl, flags=re.DOTALL)
    if match:
        attrs = match.group(1) + ", " + new_attr
        decl = decl.removeprefix(match.group(0))
    else:
        attrs = new_attr

    if docstring.strip():
        docstring = make_docstring(docstring)

    if decl.startswith("to_additive"):
        global warned_to_additive
        if not warned_to_additive:
            warned_to_additive = True
            logger.warning(
                "Encountered additive declaration(s) generated from @[to_additive]. " +
                "This script currently adds a placeholder, which is likely incorrect. You may decide to:\n" +
                "- Add only the additive declaration in the blueprint by `attribute [blueprint] additive_name`\n" +
                "- Add only the multiplicative declaration in the blueprint by `@[to_additive, blueprint]`\n" +
                "- (Current) add both in the blueprint by `@[to_additive (attr := blueprint)]`"
            )
        decl = decl.removeprefix("to_additive").strip()
        if decl:
            decl = decl + " "
        return f"to_additive (attr := {attrs}) {decl}{docstring}"

    return f"{command_modifiers}{docstring}\n@[{attrs}]\n{decl}"


def modify_source(node: Node, file: Path, location: DeclarationLocation, add_uses: bool, prepend: Optional[list[str]] = None):
    """Modify a Lean source file to add @[blueprint] attribute and docstring to the node."""
    source = file.read_text()
    pre, decl, post = split_declaration(source, location.range.pos, location.range.end_pos)
    # If there needs to be raw `uses` added, or there is `sorry`, then the inferred dependencies are incomplete, so `uses` is needed
    add_uses = add_uses or (node.proof is None and "sorry" in decl)
    add_uses_raw = add_uses or len(node.statement.uses_raw) > 0
    add_proof_uses = add_uses or (node.proof is not None and "sorry" in decl)
    add_proof_uses_raw = add_uses or (node.proof is not None and len(node.proof.uses_raw) > 0)
    attr = node.to_lean_attribute(
        add_statement_text=False, add_uses=add_uses, add_uses_raw=add_uses_raw, add_proof_uses=add_proof_uses, add_proof_uses_raw=add_proof_uses_raw
    )
    decl = insert_docstring_and_attribute(decl, new_docstring=node.statement.text, new_attr=attr)
    if prepend is not None:
        decl = "".join(p + "\n\n" for p in prepend) + decl
    file.write_text(pre + decl + post)


def add_blueprint_gen_import(file: Path):
    """Adds `import Architect` before the first import in the file."""
    source = file.read_text()
    lines = source.splitlines(keepends=True)
    first_import_index = 0
    for i, line in enumerate(lines):
        if line.startswith("import ") or line.startswith("public import "):
            first_import_index = i
            break
    lines = lines[:first_import_index] + ["import Architect\n"] + lines[first_import_index:]
    source = "".join(lines)
    file.write_text(source)


def topological_sort(data: list[tuple[NodeWithPos, str]]) -> list[tuple[NodeWithPos, str]]:
    name_to_node: dict[str, tuple[NodeWithPos, str]] = {node.name: (node, value) for node, value in data}

    visited: set[str] = set()
    result: list[tuple[NodeWithPos, str]] = []

    def visit(name: str):
        if name in visited:
            return
        visited.add(name)

        node, value = name_to_node[name]
        for used in node.uses:
            if used in name_to_node:
                visit(used)
        result.append((node, value))

    for node, _ in data:
        visit(node.name)

    return result


def write_blueprint_attributes(nodes: list[NodeWithPos], modules: list[str], root_file: str, convert_informal: bool, add_uses: bool):
    # Sort nodes by reverse position, so that we can modify later declarations first
    nodes_location_order = sorted(
        nodes,
        key=lambda n:
            (n.location.module, n.location.range.pos.line) if n.location is not None else ("", 0),
        reverse=True
    )
    nodes_topological_order = [n for n, _ in topological_sort([(n, "") for n in nodes])]

    # For upstream nodes and informal-only nodes, they are rendered as `attribute [blueprint] node_name` and
    # `theorem node_name : (sorry_using [uses] : Prop) := by sorry_using [uses]` respectively,
    # and prepended to normal nodes that directly depend on them.
    # If no such normal node exists, the upstream node is added to the root file.

    # Mapping from normal node to strings that should be prepended to it.
    prepends: dict[str, list[str]] = {n.name: [] for n in nodes}
    # The extra Lean source to be inserted somewhere in the project,
    # containing (1) upstream nodes and (2) informal-only nodes,
    # that are not directly used by any normal node in the blueprint
    extra_nodes: list[str] = []
    def is_upstream_or_informal(node: NodeWithPos) -> bool:
        return node.location is None or not any(node.location.module.split(".")[0] == module for module in modules)
    def upstream_or_informal_to_lean(node: NodeWithPos) -> str:
        if node.location is not None:
            return f"attribute [{node.to_lean_attribute()}] {node.name}"
        else:
            lean = ""
            if node.statement.text.strip():
                lean += f"{make_docstring(node.statement.text)}\n"
            lean += f"@[{node.to_lean_attribute(add_statement_text=False, add_uses=False, add_proof_text=False, add_proof_uses=False)}]\n"
            if node.proof is None:
                lean += f"def {node.name} : (sorry : Type) :=\n"
                lean += f"  sorry_using [{', '.join(node.statement.all_uses())}]"
            else:
                lean += f"theorem {node.name} : (sorry_using [{', '.join(node.proof.all_uses())}] : Prop) := by\n"
                if node.proof.text.strip():
                    lean += f"  {make_docstring(node.proof.text, indent=2)}\n"
                lean += f"  sorry_using [{', '.join(node.statement.all_uses())}]"
            return lean
    for node in nodes_topological_order:
        if is_upstream_or_informal(node):
            for normal_node in nodes_topological_order[nodes_topological_order.index(node):]:
                if not is_upstream_or_informal(normal_node) and node.name in normal_node.uses:
                    prepends[normal_node.name].append(upstream_or_informal_to_lean(node))
                    break
            else:
                extra_nodes.append(upstream_or_informal_to_lean(node))

    # Main loop for adding @[blueprint] attributes to nodes
    modified_files: set[str] = set()

    for node in nodes_location_order:
        if is_upstream_or_informal(node):
            continue
        assert node.has_lean and node.file is not None and node.location is not None
        modify_source(
            node, Path(node.file), node.location, add_uses=add_uses,
            prepend=prepends[node.name]
        )
        modified_files.add(node.file)

    for file in modified_files:
        add_blueprint_gen_import(Path(file))

    # Write extra nodes to the root file
    if extra_nodes:
        extra_nodes_file = Path(root_file)
        logger.warning(
            f"Outputting some nodes to\n  {extra_nodes_file}\n" +
            "You may want to move them to appropriate locations."
        )
        imports = "import Architect"
        if extra_nodes_file.exists():
            existing = extra_nodes_file.read_text()
        else:
            existing = ""
        extra_nodes_file.write_text(
            existing + imports + "\n\n" +
            "\n\n".join(lean for lean in extra_nodes) + "\n"
        )
