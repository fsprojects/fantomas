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

Annotate every `let` binding, even where inference would manage without it. On a function that
means every parameter and the return type; on a value it means the type:

```fsharp
let writeRow (column: int) (left: string) (right: string) : unit = ...

let extensions: Set<string> = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]
```

A written type reads as documentation, and a wrong assumption fails at the definition rather than
at a call site somewhere else. Both matter more when the reader is skimming unfamiliar code, which
is most of the time, and a reader should not have to run the inference in their head to find out
what a name holds. Modules with a signature file already state this at the boundary; annotate the
implementation as well.

This applies inside a function as much as at the top level: a local `let` in a long body is
exactly where a reader loses track of what something is.

This is guidance for code you are writing or revisiting, not a reason to sweep the codebase.
When you touch a binding for some other reason, add the annotations it is missing. Leave the
bindings you had no reason to open alone.

In a file that has a signature file, the signature file is the visibility boundary: anything it
does not list is already hidden. Do not write `let private` there. The keyword adds nothing and
suggests the `.fsi` says something it does not.

Documentation comments belong in the signature file only, never in both. A `///` in the `.fs`
alongside one in the `.fsi` is a second copy to keep in step, and the one readers and tooling see
is the signature.

## Changelog

When updating `CHANGELOG.md`, add new entries to the **end** of the relevant section (e.g. `### Fixed`), not the top. One entry per issue.

## Post-task Steps

Run these after completing a task rather than during iterative development.

### Format

```bash
dotnet fsi build.fsx -- -p FormatAll
```

### Analyzers

```bash
dotnet fsi build.fsx -- -p Analyze
```

Output goes to `analysis.sarif` in the repo root. This is safe to run whenever you need it; it no
longer saturates the machine.

**Read `analysis.sarif` afterwards.** The pipeline writes its findings there and exits 0 whatever
it found, so a run finishing tells you nothing. GitHub raises the same findings as code scanning
alerts on the pull request, which is a slower way to learn about them.

When you read it, read the results for every project you touched. Filtering the paths down to
`src/Fantomas/` looks right and silently drops `src/Fantomas.Tests/`, which does not contain that
substring. Match on `src/` and look at what comes back.
