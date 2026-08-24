# The local analyzers

The style rules for this repository live here, as analyzers rather than as prose, so that the
feedback arrives while you work instead of in review. They are ordinary F# analyzers built on
`FSharp.Analyzers.SDK`, and the `Analyze` and `AnalyzeChanged` pipelines run them alongside
`Ionide.Analyzers` and `G-Research.FSharp.Analyzers`.

| Code | Rule | Severity | Runs in |
| --- | --- | --- | --- |
| [`FANTOMAS-PIPEBACK-001`](#fantomas-pipeback-001) | No backward pipe | Error | both pipelines |
| [`FANTOMAS-PRIVATE-001`](#fantomas-private-001) | No `let private` beside a signature file | Error | both pipelines |
| [`FANTOMAS-ARMORDER-001`](#fantomas-armorder-001) | Shortest match arm first | Warning | both pipelines |
| [`FANTOMAS-ANNOTATE-001`](#fantomas-annotate-001) | Annotate every `let` binding | Warning | `AnalyzeChanged` only |
| [`FANTOMAS-XMLDOC-001`](#fantomas-xmldoc-001) | No doc comment beside a signature file | Warning | `AnalyzeChanged` only |

## FANTOMAS-PIPEBACK-001

Never write `<|`. Parenthesise instead:

```fsharp
oneAtATimePerFile request.FilePath (fun () -> task { ... })
```

It reads against the direction everything around it is written in, and it puts no visible boundary
where the argument starts.

The operator arrives in the untyped tree as a `SynExpr.LongIdent` holding the compiled name, so the
rule catches `f <| x` and the spelled out `op_PipeLeft f x` alike, and never sees a `<|` inside a
string literal.

## FANTOMAS-PRIVATE-001

In a file that has a signature file, the signature file is the visibility boundary: anything it does
not list is already hidden. Do not write `let private` there. The keyword adds nothing and suggests
the `.fsi` says something it does not.

The rule reads `ctx.ProjectOptions.SourceFiles` rather than the filesystem to decide whether a
signature file exists, because an `.fsi` that is not compiled says nothing about what is visible. It
covers `let rec private` and `let inline private` too, which is more than a grep for `let private`
manages.

## FANTOMAS-ARMORDER-001

In a `match`, put the shortest arm first:

```fsharp
match tool with
| None -> ValueNone
| Some(_, version) -> ValueSome(FantomasVersion(version.ToLowerInvariant()))
```

The short arm is nearly always the one that gets out of the way, and reading it first says what the
rest of the expression is not about. It is also the order `fsharp_experimental_keep_indent_in_branch`
wants: with the long arm last, its body can hold the indentation of the match instead of stepping in
another level. Nothing here is formatted with that setting on, but writing the arms in the order that
suits it costs nothing.

The analyzer is deliberately narrower than the rule, because arm order is semantically significant
and reordering overlapping patterns changes meaning. It speaks only for exactly two arms, with no
`when` guard, where both patterns are a top level `SynPat.LongIdent` with differing final
identifiers, and one arm is entirely on one line while the other is not. That pattern condition does
most of the work: it admits union cases and literals while excluding the wildcard and the bare
binder, which are the two patterns that match anything and so have to stay last. It stays quiet
where a comment between the arms or a conditional directive inside the match would make a swap
something other than a swap, and it offers no fix, because swapping two clauses with their trivia
and indentation is the kind of edit that goes wrong quietly.

So a match it says nothing about is not necessarily in the right order. The rule is still the rule.

## FANTOMAS-ANNOTATE-001

Annotate every `let` binding, even where inference would manage without it. On a function that means
every parameter and the return type; on a value it means the type:

```fsharp
let writeRow (column: int) (left: string) (right: string) : unit = ...

let extensions: Set<string> = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]
```

A written type reads as documentation, and a wrong assumption fails at the definition rather than at
a call site somewhere else. Both matter more when the reader is skimming unfamiliar code, which is
most of the time, and a reader should not have to run the inference in their head to find out what a
name holds. Modules with a signature file already state this at the boundary; annotate the
implementation as well. This applies inside a function as much as at the top level: a local `let` in
a long body is exactly where a reader loses track of what something is.

It is guidance for code you are writing or revisiting, not a reason to sweep the codebase, which is
why the rule runs in `AnalyzeChanged` and not in the full `Analyze`. When you touch a binding for
some other reason, add the annotations it is missing. Leave the bindings you had no reason to open
alone.

Passed over: signature files, since a `val` already states the type; the unit parameter, which has
nowhere to put one; tuple and record patterns on the left of the equals, which have no sensible
annotation to ask for; and any binding carrying a test attribute, along with everything nested
inside it. Annotating `let someTest () : unit` says nothing a reader did not already know, and the
locals in a test body are scaffolding. The test exemption keys on the attribute rather than on the
project, so there is no list of test projects to keep in step.

## FANTOMAS-XMLDOC-001

Documentation comments belong in the signature file only, never in both. A `///` in the `.fs`
alongside one in the `.fsi` is a second copy to keep in step, and the one readers and tooling see is
the signature.

The rule takes the looser of the two readings: it reports any `///` in a file that has a signature
file, because it cannot tell whether the signature documents the same binding. So a helper that
appears in neither is reported too, and the answer there is an ordinary `//` comment. That is the
convention in this folder, and it is worth knowing before writing a rule: `///` here means the
signature file.

## Severity and scope

Severity and scope are separate levers. Severity decides whether the run fails: the tool exits
non-zero on any finding at error severity, so the two error rules fail `Analyze` and fail CI. Scope
decides who has to look: a rule excluded from `Analyze` is absent from CI and from GitHub code
scanning whatever its severity, and still reports locally.

The two advisory rules are excluded from `Analyze` because both report on debt that predates them.
`AnalyzeChanged` runs them, and narrows them further to the lines `git diff` says you touched: a
file is a much coarser scope than those two rules ask for, and one line changed in a file of several
thousand otherwise surfaces every unannotated binding in it. Every other rule reports wherever it
fires in a file you edited, because a finding from one of those is worth seeing.

The narrowing reads the tool's own output format and fails open, so anything it cannot parse is
kept. A change upstream makes it stop narrowing rather than start hiding.

`AnalyzeChanged` also demotes the two error rules to warnings, because the run you do while working
should report everything and stop for nothing.

All four severities print at the tool's default verbosity, so choosing a lower one does not hide a
finding, it only stops the run failing. `Info` and `Hint` are real options for a rule that should be
seen but never block. What they do change is how GitHub code scanning classifies the alert, so the
scope column above is still the lever for a rule with existing debt.

## Suppressing a finding

Use the SDK's comment syntax rather than reshaping the code around it. The SDK filters the messages
itself, so an analyzer never has to think about this.

```fsharp
// fsharpanalyzer: ignore-line-next FANTOMAS-ANNOTATE-001
// fsharpanalyzer: ignore-file FANTOMAS-XMLDOC-001
// fsharpanalyzer: ignore-region-start FANTOMAS-PIPEBACK-001
// fsharpanalyzer: ignore-region-end
```

## Working on them

Both projects are in `fantomas.slnx`, so `dotnet build` and `dotnet test` at the repository root
cover them along with everything else, and so does the `Build` pipeline. While writing a rule,
`dotnet test analyzers/Fantomas.Analyzers.Tests` is the short loop.

They are analyzed like everything else, and come back clean. Nothing is circular about a rule
reporting on the project that defines it: the pipelines build the analyzers before running them, so
what looks at this code is the build the run started with.

They do not inherit the repository root `Directory.Build.props`. The one in this folder stops the
walk up, because they cannot restore under the repository's central package management:
`FSharp.Analyzers.SDK` pins `FSharp.Core` to a version the product does not use, which is a hard
NU1109 rather than a warning. Inheriting the root would also hand them version-less package
references that only resolve under central package management.

The two projects target different frameworks on purpose. `Fantomas.Analyzers` is `net8.0`, because
that is what the `fsharp-analyzers` tool loads, and a `net10.0` assembly fails to load when the tool
runs on the .NET 8 runtime. `Fantomas.Analyzers.Tests` is `net10.0`, like the rest of the solution,
because nothing loads it as an analyzer.

## Two ways a rule silently does nothing

Both of these produce a clean run rather than an error, so check that a new rule actually fires
rather than that the run succeeded.

- The assembly name has to contain `Analyzer`. The SDK globs `*Analyzer*.dll` and looks at nothing
  else. This is also why `--analyzers-path` is given the analyzer project's own output folder rather
  than `analyzers`: the search is recursive, and it would otherwise pick up
  `Fantomas.Analyzers.Tests.dll` as well, whose name slips past the SDK's own test exclusion.
- The `FSharp.Analyzers.SDK` version has to track the `fsharp-analyzers` version pinned in
  `.config/dotnet-tools.json`. Bump them together.

## Writing a rule

Every rule is registered twice, as a `CliAnalyzer` and as an `EditorAnalyzer`, with both attributes
in the signature file. The pipelines use the first; the second is what makes the rule show up in
Ionide as you type. Between them the five rules read exactly three things, `ctx.FileName`,
`ctx.ProjectOptions.SourceFiles` and `ctx.ParseFileResults.ParseTree`, all of which `EditorContext`
carries as well as `CliContext` does. Reach for the typed tree and that stops being true, and the
editor registration has to go.

Where a rule needs to know whether a file has a signature file, ask `ctx.ProjectOptions.SourceFiles`
rather than the filesystem. An `.fsi` that is not compiled says nothing about what is visible, and a
test that builds its sources in memory has no filesystem to look at.

A new rule needs a section above, because `HelpUri` links to it. Anything a person is told to do
belongs there rather than only in the message.

Tests live in `Fantomas.Analyzers.Tests` and go through `cliAnalyzer`, using
`FSharp.Analyzers.SDK.Testing` to build a real `CliContext`. That is deliberately the entry point
the pipelines use, so the wiring is covered rather than bypassed: read `ctx.FileName` or
`ctx.ProjectOptions.SourceFiles` wrongly and a test notices. `analyzeSource` covers a single
snippet, `analyzeWithSignature` builds an implementation with a signature file beside it, which is
what the two rules keyed on the signature file need. A snippet has to begin with a module
declaration, because the harness type checks it as part of a project and raises on any compiler
error. Give every rule a test for the finding and a test for each shape that looks like it but is
not.

Two things about that harness are worth knowing, because both have already cost a day.

`mkOptionsFromProject` is not cheap or hermetic. It runs `dotnet new classlib` and `dotnet build` in
a temporary folder and reads the binlog, caching it in the temp directory. The framework it is given
has to be one the machine can actually build: it was `net8.0` first, which passed on a developer
machine with an old SDK lying around and failed every test in the dev container, which carries only
.NET 10. It is `net10.0` now, matching `global.json`, so it works wherever this repository builds at
all. It also catches its own failures and hands back empty options, which surfaces later as an
exception about critical errors in the project options and names nothing useful, so the fixture
checks the options came back non-empty and says so plainly if they did not.

The options it returns are a fresh project's defaults, not this repository's. Of everything
`Directory.Build.props` adds, only `--strict-indentation+` reaches the parser, and the fixture
appends it. `--realsig+` and the `--test:` switches are for later compiler phases and cannot change
a tree, and LangVersion is never set here at all. If you want to check that for yourself, a design
time build prints the real command line in about half a second:

```bash
dotnet msbuild src/Fantomas.Core/Fantomas.Core.fsproj -t:CoreCompile \
  -p:SkipCompilerExecution=true -p:ProvideCommandLineArgs=true \
  -p:BuildProjectReferences=false -p:DesignTimeBuild=true -getItem:FscCommandLineArgs
```

None of this is currently load bearing. Parsing every file of `Fantomas.Core` under the default
options, both define sets, `--strict-indentation+` and `--langversion:preview` produces identical
findings from all five rules, which is what you would expect of rules that read only the untyped
tree. The defines are the only option that could change a verdict, since they decide which branch of
an `#if` reaches the tree, and `DEBUG` in `Selection.fs` is the only one in real source anywhere.
