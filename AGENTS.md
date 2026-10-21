# Fantomas

F# source code formatter. Parses F# to an untyped AST (via vendored FCS), transforms it to an intermediate representation called Oak (`SyntaxOak.fs`), then prints it back via writer events (`CodePrinter.fs` + `Context.fs`).

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

## Talking about a formatting change

### Trivia versus comments in our code

"Comment" alone is ambiguous here, so always name which kind:

- **Trivia** is what sits in the F# that Fantomas reads or writes: a trivia line comment, a trivia
  block comment, a blank line, a directive. A comment in a formatted result is always trivia.
- In Fantomas' own source, say **explanatory comment** (`//` saying why the code does what it does)
  or **doc comment** (`///`).

Keep the explanatory and doc comments beside the code a change touches accurate as part of every
change, and correct them without asking: re-read them after editing, and say in your report what you
corrected. Comments elsewhere in the file stay as they are.

An explanatory comment sits beside the code it explains. When it explains one branch of an `if` or a
`match`, it goes at the top of that branch, rather than in a block above the whole expression that
explains every branch at once.

When it explains a layout, show the layout instead of describing it: a few lines of the F# the code
produces, with a short note on the line that matters.

### Every example in full

The user answers from the message alone, without the code base open. Every finding, review point or
question about formatting carries a full example:

1. The input, as a user would write it: a whole, parseable declaration, never a fragment.
2. What main formats it to, or what the branch did before this change (say which). When the result
   is not idempotent, every pass until it settles.
3. What this branch formats it to.
4. What is wrong or what changed, marked with an F# `// <-` comment on the exact lines, and the
   compiler error when it does not compile.
5. For a decision: every option as complete code, each saying whether it compiles and is
   idempotent, then the question, with an easy answer (A/B, yes/no).

Say which examples are real output and which are written by hand. Fantomas' parser accepts some
layouts the shipped compilers reject, so check "it compiles" with the SDK compilers: a `.fsx` in its
own folder under `tmp/`, a `global.json` pinning the SDK, then `dotnet fsi`.

Repeat context rather than refer back to earlier messages, files, section numbers or review point
numbers. When where the printer does it helps, say it in plain words with enough context to follow
without opening `CodePrinter.fs`. After a review, first one line on what it found, then one numbered
question at a time.

### question.fs and review.fs

When a question needs F# code to show it, because it is about what Fantomas formats code to or
about a formatting bug, write it into `question.fs` in the repo root, left untracked: the chat wraps
long lines, and the user reads code in the editor. Questions about anything else (the docs site,
tooling, staging) stay in the chat.

A review's findings that come with F# code, nits included, go into `review.fs` the same way, as
soon as the review is in: every finding with its full example, ending on the first question.

Either file:

- opens with the situation in a few sentences and ends with the question;
- holds the prose as trivia block comments, the `// <-` markers as trivia line comments and every
  example as code, so it is the single source of truth;
- parses as a whole: give each block its own name (`Input`, `OptionA`) and say at the top what the
  real name is; check it with `scripts/ast.fsx`.

`.fantomasignore` lists both, so the formatter hook and the editor leave their examples as written.
The chat message gives the path and the question in one line. Once the user has answered the last
question in it, delete the file before doing anything else.

## Code Style

The style rules for this repository are analyzers rather than prose, so the feedback arrives while
you work instead of in review. They live in `analyzers/`, and the `Analyze` and `AnalyzeChanged`
pipelines run them alongside the two analyzer packages.

[analyzers/AGENTS.md](analyzers/AGENTS.md) lists them and has what each one asks for and why, how to
suppress a finding, and what to know before writing another. Every finding links to its own section
there. `dotnet fsi build.fsx -- -p AnalyzeChanged` will tell you the same thing about the code in
front of you.

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
