---
description: Investigate and fix a Fantomas formatting issue from GitHub
---

The input is a Fantomas GitHub issue URL (e.g. https://github.com/fsprojects/fantomas/issues/1234).

Follow these steps in order:

## 1. Fetch the issue

Use `gh issue view <number> --repo fsprojects/fantomas --json title,body,labels` to get the issue details. Extract the example code and expected behavior. Note the labels: `bug (soundness)` vs `bug (stylistic)` affects the changelog entry.

## 2. Reproduce the problem

Use the /format skill to format the example code and confirm the bug exists. If confirmed, try to trim the example down to the minimal reproduction case.

## 3. Add a failing snapshot case

A formatting test is a snapshot case in `src/Fantomas.Core.SnapshotTests/cases/`. Read that
project's `README.md` first: it says where a case goes, how to name it and what every case checks.
In short:

- Look for a case that already pins the bug: a `*.ignore.fs` in the node's folder, or one that
  names the issue. Renaming it to `name.fs` gives the failing case, golds included.
- Put the input in the folder of the node it is about, `cases/oak/<Union>/<Case>/` or
  `cases/oak/<Node>/`, or under `cases/settings/<key>/` when the point is what a setting does. A
  case about comments, blank lines or directives goes in the node's `trivia/` folder. One that
  formatting must leave as it is goes in `negative/`, and is its own gold.
- Name it in lower case words joined by dashes, the issue number first:
  `1234-comment-after-the-arrow.fs`. Variations of the original report need no number.
- Settings go in front matter on top of the input:

```fsharp
(*---
fsharp_multiline_bracket_style = stroustrup
---*)
let x = ...
```

- Write `name.gold.fs` by hand with the output the issue asks for. With `#if` in the input, each
  define combination has a gold too (`name.no-defines.gold.fs`, `name.DEBUG.gold.fs`), and
  `name.gold.fs` holds the merged result.

### Verify signature files

Check if the fix should also apply to signature files (`*.fsi`). If so, add a `.fsi` case beside
the `.fs` one, or try the input with `scripts/format.fsx --signature`.

### Verify slight variations

Check if additional cases are needed for different setting combinations or define combinations.

Run the case with `dotnet test src/Fantomas.Core.SnapshotTests --filter "Name~1234"` and **assert
it fails** before proceeding to the fix.

## 4. Investigate the root cause

Use the /ast, /oak, /trivia and /writer-events skills to understand what's happening. When the
syntax tree lacks a range or a flag, the parser that builds it is in `.deps/<hash>/src/Compiler/`:
`pars.fsy` and `SyntaxTree/ParseHelpers.fs`. Key files to inspect:
- `src/Fantomas.Core/CodePrinter.fs` - the main printer
- `src/Fantomas.Core/Context.fs` - writer context
- `src/Fantomas.Core/ASTTransformer.fs` - AST to Oak transformation
- `src/Fantomas.Core/Trivia.fs` - trivia (comments, blank lines, directives)

## 5. Implement the fix

Make the minimal change needed. Run the new case to confirm it passes.

## 6. Run all tests

Run `dotnet test src/Fantomas.Core.SnapshotTests` and `dotnet test src/Fantomas.Core.Tests`. If many cases fail, the fix is likely too broad: make it more targeted. If only a few fail and the new behavior is arguably better, accept their new golds with `FANTOMAS_UPDATE_SNAPSHOTS=1` and a filter on their names, and ask the user for their opinion (`git diff` of the golds is easiest to review).

## 7. Update CHANGELOG.md

Add an entry under the `## [Unreleased]` section. Never add to an already-published version section. If there is no `Unreleased` section, create one at the top above the most recent version.

- For `bug (soundness)` fixes, add under `### Fixed` using the original issue title:
  `- <Original GitHub issue title>. [#1234](https://github.com/fsprojects/fantomas/issues/1234)`
- For `bug (stylistic)` fixes (not related to a style guide), also add under `### Fixed`.
- For `bug (stylistic)` fixes related to a style guide, add under `### Changed`:
  `- Update style of xyz. [#1234](https://github.com/fsprojects/fantomas/issues/1234)`

## 8. Post-task steps

Run `dotnet fsi build.fsx -- -p FormatChanged`, then `dotnet fsi build.fsx -- -p AnalyzeChanged`,
and read `analysis.sarif` afterwards: the analyzer run exits 0 whatever it found. The Post-task
Steps section of `AGENTS.md` says what each one covers.
