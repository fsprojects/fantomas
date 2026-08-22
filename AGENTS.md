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

In a `match`, put the shortest arm first:

```fsharp
match tool with
| None -> ValueNone
| Some(_, version) -> ValueSome(FantomasVersion(version.ToLowerInvariant()))
```

The short arm is nearly always the one that gets out of the way, and reading it first says what the
rest of the expression is not about. It is also the order `fsharp_experimental_keep_indent_in_branch`
wants: with the long arm last, its body can hold the indentation of the match instead of stepping in
another level. Nothing here is formatted with that setting on, but writing the arms in the order
that suits it costs nothing.

Never write `<|`. Parenthesise instead:

```fsharp
oneAtATimePerFile request.FilePath (fun () -> task { ... })
```

It reads against the direction everything around it is written in, and it puts no visible boundary
where the argument starts.

## Changelog

When updating `CHANGELOG.md`, add new entries to the **end** of the relevant section (e.g. `### Fixed`), not the top. One entry per issue.

## Post-task Steps

Run these after completing a task rather than during iterative development.

### Format

```bash
dotnet fsi build.fsx -- -p FormatChanged
```

This formats the F# files the working tree changed, which is what a task normally touches. To
format everything, including the docs and the build script:

```bash
dotnet fsi build.fsx -- -p FormatAll
```

### Analyzers

```bash
dotnet fsi build.fsx -- -p AnalyzeChanged
```

This analyzes the files the working tree changed, and nothing else. A project is loaded when it
owns a changed `.fs` or `.fsi`, and is then analyzed for those files alone. A changed `.fsproj`
asks for the whole project, because what it compiles is no longer what it compiled before.

Scoping it to the changed files is what makes this quick: analyzing one file of
`Fantomas.Core.Tests` takes seconds where the whole project takes minutes.

```bash
dotnet fsi build.fsx -- -p Analyze
```

This analyzes every file of every project. The test projects are the largest of the solution and
decide how long that takes: the smallest projects report within seconds, `Fantomas.Core.Tests`
takes a couple of minutes. Run it before opening a pull request, and while working use
`AnalyzeChanged`, which cannot see a finding your change causes in a file you did not edit.

Both pipelines analyze each project in its own process, so findings are printed per project as
that project finishes rather than all at the end.

The findings also land in `analysis.sarif` in the repo root, merged from the per-project reports in
`analysisreports/`. Both files hold the last run and nothing more, so after `AnalyzeChanged` they
cover only the files it looked at. **Read one of them afterwards.** The pipeline exits 0 whatever
it found, so a run finishing tells you nothing. GitHub raises the same findings as code
scanning alerts on the pull request, which is a slower way to learn about them.

When you read the SARIF, read the results for every project you touched. Filtering the paths down
to `src/Fantomas/` looks right and silently drops `src/Fantomas.Tests/`, which does not contain
that substring. Match on `src/` and look at what comes back.

`Fantomas.FCS` and `Fantomas.FCS.BuildTasks` are left out: both are vendored compiler sources, so a
finding in either is something to report upstream rather than something to fix here.
