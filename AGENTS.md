# Fantomas

F# source code formatter. Parses F# to an untyped AST (via vendored FCS), transforms it to an intermediate representation called Oak (`SyntaxOak.fs`), then prints it back via writer events (`CodePrinter.fs` + `Context.fs`).

## Build & Test

```bash
dotnet build fantomas.slnx
dotnet test src/Fantomas.Core.Tests/
```

## Diagnostic Scripts

All of these accept a file path or stdin, with optional `--signature` and `--editorconfig <content>` flags.

- `scripts/ast.fsx` — untyped AST
- `scripts/oak.fsx` — Oak tree
- `scripts/format.fsx` — format with local build
- `scripts/writer-events.fsx` — writer events produced during formatting
- `scripts/chain.fsx` - ExprChain structure (head, segments, terminal); ignores `--editorconfig`

Scripts require a debug build first (`dotnet build src/Fantomas/Fantomas.fsproj`).

## Code Style

The style rules for this repository are analyzers rather than prose, so the feedback arrives while
you work instead of in review. They live in `analyzers/`, and the `Analyze` and `AnalyzeChanged`
pipelines run them alongside the two analyzer packages.

| Code | Rule | Severity |
| --- | --- | --- |
| `FANTOMAS-PIPEBACK-001` | No backward pipe | Error |
| `FANTOMAS-PRIVATE-001` | No `let private` beside a signature file | Error |
| `FANTOMAS-ARMORDER-001` | Shortest match arm first | Warning |
| `FANTOMAS-BRANCHORDER-001` | Shortest `if` branch first | Warning |
| `FANTOMAS-KEEPINDENT-001` | Last branch keeps the indentation | Warning |
| `FANTOMAS-ANNOTATE-001` | Annotate every `let` binding | Warning |
| `FANTOMAS-XMLDOC-001` | No doc comment the signature file already carries | Warning |

[analyzers/AGENTS.md](analyzers/AGENTS.md) has what each one asks for and why, how to suppress a
finding, and what to know before writing another. `dotnet fsi build.fsx -- -p AnalyzeChanged` will
tell you the same thing about the code in front of you.

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

Nothing here fails the run, and everything it reports is a finding in a file `git status` names as
changed. A finding in a file you did not touch is dropped, whatever its rule: without that, a
changed `.fsproj` pulling in the whole project buries the two files you added under the project's
existing debt.

Within a file that did change, `FANTOMAS-ANNOTATE-001` is narrowed further to the lines `git diff`
says you touched. A file is a much coarser scope than that rule asks for: one line changed in
`ASTTransformer.fs` otherwise surfaces every unannotated binding in it. Every other rule reports
wherever it fires in a file you edited, including `FANTOMAS-KEEPINDENT-001`, which also reports on
pre-existing debt but means an arm's body rather than the `|` lines a swap touches. A file git has
never seen is new in its entirety, so everything in it is reported.

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
cover only the files it looked at. **Read one of them afterwards.** `AnalyzeChanged` exits 0
whatever it found, so a run finishing tells you nothing. `Analyze` does fail on a finding at error
severity, which today means the two local rules that carry it. GitHub raises everything else as
code scanning alerts on the pull request, which is a slower way to learn about them.

When you read the SARIF, read the results for every project you touched. Filtering the paths down
to `src/Fantomas/` looks right and silently drops `src/Fantomas.Tests/`, which does not contain
that substring. Match on `src/` and look at what comes back.

`Fantomas.FCS` and `Fantomas.FCS.BuildTasks` are left out: both are vendored compiler sources, so a
finding in either is something to report upstream rather than something to fix here.
