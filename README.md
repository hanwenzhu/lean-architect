# lean-architect

Lean-architect is a tool for generating the blueprint data of a Lean project directly from Lean.

The blueprint is a high-level plan of a Lean project, consisting of a series of nodes (theorems and definitions) and the dependency relations between them.
The purpose of lean-architect is to make it easier to write the blueprint by generating blueprint data directly from Lean.

Start by annotating definitions and theorems in Lean with the `@[blueprint]` tag. They will then be exported to LaTeX, which you may then put in the blueprint.

This tool is built to complement [leanblueprint](https://github.com/PatrickMassot/leanblueprint) and its structure is inspired by [doc-gen4](https://github.com/leanprover/doc-gen4). The idea is inspired by [leanblueprint-extract](https://github.com/AlexKontorovich/PrimeNumberTheoremAnd/tree/main/leanblueprint-extract).

## Instructions

First, install [leanblueprint](https://github.com/PatrickMassot/leanblueprint) and follow the instructions there to set up a blueprint project using `leanblueprint new`, if not already done.

Add lean-architect to the lakefile. For example:

```toml
[[require]]
name = "lean-architect"
git = "https://github.com/hanwenzhu/lean-architect.git"
rev = "main"
```

To extract the blueprint for a module, first `import Architect` and then annotate key theorems and definitions in the file with `@[blueprint]`:

```lean
import Architect

@[blueprint]
theorem my_theorem : Foo Bar := by foo
```

(See also a full example below.)

Then input the extracted blueprint source to the blueprint document (typically, `blueprint/src/content.tex`):

```latex
% This makes the macros `\inputleanmodule` and `\inputleannode` available.
\input{../../.lake/build/blueprint/library/Example}

% Input the blueprint contents of module `Example.MyNat`:
\inputleanmodule{Example.MyNat}

% You may also input only a single node using:
% \inputleannode{MyNat.add}.
```

Then run:

```sh
# Generate the blueprint to .lake/build/blueprint
lake build :blueprint
# Build the blueprint using leanblueprint
leanblueprint pdf
leanblueprint web
```

If you see LaTeX errors here, you may need to manually fix some docstrings so that the extracted LaTeX compiles.

(See also the instructions for converting from an existing blueprint below.)

## Example

This example is hosted at [lean-architect-example](https://github.com/hanwenzhu/lean-architect-example). Consider the following `MyNat` API:

```lean
-- Example/MyNat.lean

/-! # Natural numbers -/

@[blueprint "Natural numbers"]
inductive MyNat : Type where
  | zero : MyNat
  | succ : MyNat → MyNat

namespace MyNat

/-!
## Addition
Here we define addition of natural numbers.
-/

/-- Natural number addition. -/
@[blueprint]
def add (a b : MyNat) : MyNat :=
  match b with
  | zero => a
  | succ b => succ (add a b)

/-- For any natural number $a$, $0 + a = a$, where $+$ is Def. `MyNat.add`. -/
@[blueprint, simp]
theorem zero_add (a : MyNat) : add zero a = a := by
  /-- The proof follows by induction. -/
  induction a <;> simp [*, add]

/-- For any natural numbers $a, b$, $(a + 1) + b = (a + b) + 1$. -/
@[blueprint]
theorem succ_add (a b : MyNat) : add (succ a) b = succ (add a b) := by
  /-- Proof by induction on `b`. -/
  -- If the proof contains sorry, the `\leanok` command will not be added
  sorry

/-- For any natural numbers $a, b$, $a + b = b + a$. -/
@[blueprint]
theorem add_comm (a b : MyNat) : add a b = add b a := by
  induction b with
  | zero =>
    -- The inline code `abc` is converted to \ref{abc} if possible.
    /-- The base case follows from `MyNat.zero_add`. -/
    simp [add]
  | succ b ih =>
    /-- The inductive case follows from `MyNat.succ_add`. -/
    sorry_using [succ_add]  -- the `sorry_using` tactic declares that the proof uses succ_add

-- Additional content omitted

end MyNat
```

The (automatic) output of the above example Lean script is:

![Blueprint web](https://raw.githubusercontent.com/hanwenzhu/lean-architect-example/refs/heads/main/images/web.png)

With dependency graph:

![Depedency graph](https://raw.githubusercontent.com/hanwenzhu/lean-architect-example/refs/heads/main/images/depgraph.png)

## Specifying the blueprint

After tagging with `@[blueprint]`, lean-architect will:

1. Extract the statement and proof of a node from docstrings.
2. Infer the dependencies of a node from the constants used in the statement or proof.
3. Infer whether the statement or proof is ready (i.e. `\leanok`) from whether it is sorry-free.
4. Add the node to the extracted blueprint.

You may override the constants used in the statement or proof with the `uses` and `proofUses` options, or with the `using` tactic.

To view the extracted blueprint data of a node, use `@[blueprint?]`.

The Markdown [docstrings](https://leanprover-community.github.io/contribute/doc.html) will be automatically parsed and converted to LaTeX.
Citations are supported using square brackets like `[wiles1995]`, and references to other Lean nodes can be done by inline code like `` `Lean.theorem_name` ``. The output can be modified by the options `blueprint.bracketedCitations`, `blueprint.refCommand` and `blueprint.citeCommand`. Raw LaTeX is also supported.

## Informal-only nodes

Especially at the start of a project, theorems or definitions may be written in LaTeX, and their statements are not ready to be formalized in Lean.
Lean-architect supports mixing such *informal* nodes written in LaTeX with *formal* nodes written in Lean. Typically, the workflow of an entire project may look like this:

1. Write a blueprint in LaTeX/Overleaf
2. Set up a new Lean project with this blueprint
3. Formalize a theorem `my_theorem` from LaTeX into Lean, and tag it with `@[blueprint]`
4. Replace this theorem in LaTeX with `\inputleannode{my_theorem}`, and return to (3)

One utility script for automating this replacement is (although you may prefer to do this manually):

```sh
# Convert from a LaTeX node that has a Lean corresponding part (i.e. with `\lean`)
# to a `\inputleannode` command, and try to automatically tag the Lean part with
# `@[blueprint]`.
lake script run blueprintConvert --nodes <Lean name of node>
```

One specific case to note is when Lean theorems need to reference a informal theorem (say with label `\label{thm}`). Here, you may write the label in double quotes, such as `@[blueprint (uses := ["thm"])]`.

## Converting from existing blueprint format

With a project that uses the existing leanblueprint format, there is a primitive script that tries to convert to the lean-architect format.

Currently, this script depends on a recent version of Python with `loguru` and `pydantic` installed (install by `pip3 install loguru pydantic`); and requires an installation of [Pandoc](https://pandoc.org) to be available.

First go to a clean branch **without any uncomitted changes**, to prevent overwriting any work you have done.

You can then convert to lean-architect format by adding `lean-architect` as a dependency to lakefile, run `lake update lean-architect`, ensure `leanblueprint checkdecls` works (i.e. all `\lean` are in Lean), and then run:

```sh
lake script run blueprintConvert
```

Note that this conversion is not perfect and not idempotent, and for large projects it may end in some small syntax errors. You would need to fix the errors in the converted files.

The informal-only nodes (nodes without `\lean`) are by default retained in LaTeX and not converted to Lean. If you want them to be converted, you may add `--convert_informal` to the command above, and then the script will convert them and save to the root Lean module.

The conversion will remove the `\uses` information in LaTeX and let lean-architect automatically infer dependencies in Lean, unless the code contains `sorry` (in which case `uses :=` and `proofUses :=` will be added). If `--add_uses` is specified then all `\uses` information is retained in Lean.

Docstrings are converted from LaTeX to Markdown using Pandoc. If there is informal description of a theorem in LaTeX and a docstring in Lean, they are concatenated to form the new docstring. You should tidy the existing Markdown docstrings (e.g. wrap code in backticks and math in dollar signs) for better rendering.

You may use `--blueprint_root <root>` to specify the path to your blueprint, if it is not the default.

(For reference, it takes a few minutes to convert [FLT](https://github.com/ImperialCollegeLondon/FLT) to lean-architect format and fix all errors, and it might take longer to fix all warnings and make the output look nicer.)

## GitHub Actions integration

If building the blueprint is part of the GitHub CI action, then you need to run `lake build :blueprint` before building the blueprint,
so that the `\input` line above works. Here are some typical examples for doing this:

- If you use `.github/workflows/blueprint.yml` from leanblueprint, then add the following step:

```yaml
      # Before "Build blueprint and copy to `home_page/blueprint`":
      - name: Extract blueprint
        run: ~/.elan/bin/lake build :blueprint
```

- If you use `.github/workflows/build-project.yml` from LeanProject, then add this `build-args` option to `leanprover/lean-action`:

```yaml
      - name: Build the project
        uses: leanprover/lean-action@...
        with:
          use-github-cache: false
          build-args: :blueprint
```

## Extracting nodes in JSON

To extract the blueprint nodes in machine-readable format, run:

```sh
lake build :blueprintJson
```

The output will be in `.lake/build/blueprint`.

## TODO

- Currently the LaTeX output (and hence PDF / web outputs) are only in a state of barely working, because it is difficult to translate Markdown to LaTeX. The immediate next improvement will be to explore supporting Verso docstrings (leanprover/lean4#10307), and more specifically, for statement / proof docstrings using `doc.verso`, to generate the LaTeX directly from Verso instead of converting to Markdown and then to LaTeX. One roadblock here is support for citations, which we may have to wait until there is a good solution (e.g. via an extension) that works for both doc-gen4 and our purpose.
