# Fantomas

F# source code formatter. Parses F# to an untyped AST (via vendored FCS), transforms it to an intermediate representation called Oak (`SyntaxOak.fs`), then prints it back via writer events (`CodePrinter.fs` + `Context.fs`).

## Build & Test

```bash
dotnet build fantomas.slnx
dotnet test src/Fantomas.Core.Tests/
```

## Diagnostic Scripts

All scripts accept a file path or stdin, with optional `--signature` and `--editorconfig <content>` flags.

- `scripts/ast.fsx` — untyped AST
- `scripts/oak.fsx` — Oak tree
- `scripts/format.fsx` — format with local build
- `scripts/writer-events.fsx` — writer events produced during formatting
- `scripts/chain.fsx` - ExprChain structure (head, segments, terminal); ignores `--editorconfig`

Scripts require a debug build first (`dotnet build src/Fantomas/Fantomas.fsproj`).

## Code Style

Annotate every parameter and the return type on a function, even where inference would manage
without it:

```fsharp
let writeRow (column: int) (left: string) (right: string) : unit = ...
```

A written signature reads as documentation, and a wrong assumption fails at the definition
rather than at a call site somewhere else. Both matter more when the reader is skimming
unfamiliar code, which is most of the time. Modules with a signature file already state this at
the boundary; annotate the implementation as well.

This is guidance for code you are writing or revisiting, not a reason to sweep the codebase.
When you touch a function for some other reason, add the annotations it is missing. Leave the
functions you had no reason to open alone.

## Changelog

When updating `CHANGELOG.md`, add new entries to the **end** of the relevant section (e.g. `### Fixed`), not the top. One entry per issue.

## Post-task Steps

Run these after completing a task, not during iterative development — analyzers can be slow.

### Format

```bash
dotnet fsi build.fsx -- -p FormatAll
```

### Analyzers

```bash
dotnet fsi build.fsx -- -p Analyze
```

Output goes to `analysis.sarif` in the repo root.
