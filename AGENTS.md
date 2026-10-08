# Fantomas

F# source code formatter. Parses F# to an untyped AST (via vendored FCS), transforms it to an intermediate representation called Oak (`SyntaxOak.fs`), then prints it back via writer events (`CodePrinter.fs` + `Context.fs`).

`docs/docs/contributors/The Formatting Pipeline.md` has every stage of `CodeFormatterImpl.formatDocument`
and how tests look inside it: through its result and its `inspect` hook, rather than through functions
exported from a `.fsi` for them.

## Build & Test

```bash
dotnet build fantomas.slnx
dotnet test src/Fantomas.Core.SnapshotTests/
dotnet test src/Fantomas.Core.Tests/
```

Formatting is tested by snapshot cases, files under `src/Fantomas.Core.SnapshotTests/cases/`, each
beside the result formatting gives for it; that project's README says how to write one.
`Fantomas.Core.Tests` holds the unit tests of internals.

## Diagnostic Scripts

All of these accept a file path or stdin, with optional `--signature` and `--editorconfig <content>` flags.
A path that does not exist exits 1 rather than falling back to stdin.

- `scripts/ast.fsx` - untyped AST
- `scripts/oak.fsx` - Oak tree
- `scripts/format.fsx` - format with local build; `--define A,B` (or `no-defines`) prints that one define combination before the merge
- `scripts/writer-events.fsx` - writer events produced during formatting
- `scripts/chain.fsx` - ExprChain structure (head, segments, terminal); ignores `--editorconfig` and the settings in a case's front matter
- `scripts/trivia.fsx` - where each piece of trivia landed: node, token, side and kind

A snapshot case (`src/Fantomas.Core.SnapshotTests/cases/`) can be passed as it is: its front matter
is read as its settings. `scripts/format.fsx` formats the way a case is formatted and reports every
problem the snapshot tests find in the result, exiting 1 when there is one; with `--define` it only
prints that combination and checks nothing. Whether the case sits in the right folder and matches
its golds, only running the case says.

Scripts require a debug build first (`dotnet build src/Fantomas.Core.SnapshotTests`): they reference
the snapshot test assembly, and building it builds Fantomas.Core, Fantomas.EditorConfig and
Fantomas.FCS too.

## Breaking changes to the Oak

The node types in `SyntaxOak.fs` are public, but a breaking change to them is fine in any release,
a patch release included. Change a node's constructor or members however a fix needs, without a
compatibility overload, and without calling it out as a breaking change. Leave Oak changes out of
`docs/docs/end-users/UpgradeGuide.md`: its "The Oak" section tells readers to diff `SyntaxOak.fs`
between tags instead. Code generation on top of the Oak is told to expect
this (`docs/docs/end-users/GeneratingCode.fsx`, "Updates").

## Code Style

The style rules for this repository are analyzers rather than prose, so the feedback arrives while
you work instead of in review. They live in `analyzers/`, and the `Analyze` and `AnalyzeChanged`
pipelines run them alongside the two analyzer packages.

[analyzers/AGENTS.md](analyzers/AGENTS.md) lists them and has what each one asks for and why, how to
suppress a finding, and what to know before writing another. Every finding links to its own section
there. `dotnet fsi build.fsx -- -p AnalyzeChanged` will tell you the same thing about the code in
front of you.

One judgement call no analyzer makes: treat `List.rev` as a smell. An accumulator built backwards
and reversed at the end is an extra pass over the list, and usually wants a list expression that
yields in order (with a local `mutable` for the running state), `List.choose` or `List.mapFold`.
Keep `List.rev` where reversing is the point.

## Changelog

When updating `CHANGELOG.md`, add new entries to the **end** of the relevant section (e.g. `### Fixed`), not the top. One entry per issue.

An entry ends in a link to the issue it closes, and to the pull request only when no issue lies
behind the change. `docs/docs/contributors/Pull request ground rules.md` is where that convention
is written down.

A pull request has no number until it is opened, so an entry that needs one is written last, in a
commit of its own:

1. Commit the work, leaving every `CHANGELOG.md` out of it.
2. Push, and open the pull request.
3. Write the entry against that pull request's URL, and commit it on its own.

Wait for the URL rather than guessing the number. An entry that links an issue needs none of this
and can be written with the work.

`src/Fantomas.Client/CHANGELOG.md` is a second changelog, covering that package alone.

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
asks for the whole project, because what it compiles is no longer what it compiled before. A
changed `.fsx` is reported on through the `Scripts` target.

Scoping it to the changed files is what makes this quick: a project is checked file by file, so a
few files of it take a fraction of the whole.

Everything it reports is about the code in front of you: findings in files you did not touch are
dropped, and a few rules that report on pre-existing debt are narrowed to the lines `git diff` says
you touched. Which rules and why is in `scripts/BuildAnalyzers.fsx`, and you do not need to know it
to act on a run. What comes out is the thing to fix.

```bash
dotnet fsi build.fsx -- -p Analyze
```

This analyzes every file of every project, and the scripts. The projects are analyzed side by side,
so the slowest decides how long that takes: the smallest report within seconds, the scripts in about
fifteen, and `Fantomas.Core`, the slowest, in about a minute. Run it before opening a pull request,
and while working use `AnalyzeChanged`, which cannot see a finding your change causes in a file you
did not edit.

The scripts are a weaker check than the projects, for reasons that are all downstream of the typed
tree a script gets. [analyzers/AGENTS.md](analyzers/AGENTS.md) has what that costs and why.

Both pipelines analyze each project in its own process, so findings are printed per project as
that project finishes rather than all at the end.

The findings also land in `analysis.sarif` in the repo root, merged from the per-project reports in
`analysisreports/`. Both files hold the last run and nothing more, so after `AnalyzeChanged` they
cover only the files it looked at. **Read one of them afterwards.** `AnalyzeChanged` exits 0
whatever it found, so a run finishing tells you nothing. `Analyze` does fail on a finding at error
severity. GitHub raises everything else as code scanning alerts on the pull request, which is a
slower way to learn about them.

When you read the SARIF, read the results for every project you touched. Filtering the paths down
to `src/Fantomas/` looks right and silently drops `src/Fantomas.Tests/`, which does not contain
that substring. Match on `src/` and look at what comes back.

`Fantomas.FCS` and `Fantomas.FCS.BuildTasks` are left out: both are vendored compiler sources, so a
finding in either is something to report upstream rather than something to fix here.
