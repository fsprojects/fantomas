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
