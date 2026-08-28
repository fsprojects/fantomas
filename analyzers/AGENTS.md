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
| [`FANTOMAS-BRANCHORDER-001`](#fantomas-branchorder-001) | Shortest `if` branch first | Warning | both pipelines |
| [`FANTOMAS-KEEPINDENT-001`](#fantomas-keepindent-001) | Last branch keeps the indentation | Warning | both pipelines |
| [`FANTOMAS-ANNOTATE-001`](#fantomas-annotate-001) | Annotate every `let` binding | Warning | `AnalyzeChanged` only |
| [`FANTOMAS-XMLDOC-001`](#fantomas-xmldoc-001) | No doc comment the signature file already carries | Warning | both pipelines |
| [`FANTOMAS-OPENS-001`](#fantomas-opens-001) | No `open` nothing in the file uses | Warning | both pipelines |

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
wants, which the repository's `.editorconfig` turns on: with the long arm last, its body can hold the
indentation of the match instead of stepping in another level. That is what lets a second `match` in
the final arm sit at the indentation of the first rather than one level in, which is worth reaching
for when one lookup falls through to another.

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

## FANTOMAS-BRANCHORDER-001

The same for an `if`. Put the shorter branch first, negating the condition to get there:

```fsharp
if not contentChanged then
    return FormatResult.Unchanged(filename = formatParams.File)
else
    let! validation = CodeFormatter.ValidateFSharpCodeAsync(isSignatureFile, formattedContent)
    ...
```

This asks for more than `FANTOMAS-ARMORDER-001` does. A match arm can only be moved, where a branch
has to be negated as well, so the rule is doing something to the condition and not only to the
layout. What it does not have to worry about is overlap: the two branches of an `if` are exclusive by
construction, so the swap is always sound, where reordering two match arms need not be.

What is not always an improvement is the condition it leaves behind. A comparison flips into its
opposite and an existing `not` falls away, and both of those are still one thing to read. A condition
joined by `&&` or `||` would have to grow a `not` and a pair of parentheses around the whole of it,
which is a worse sentence than the branches were worth, so those are left alone. Everything else can
only gain a `not`, which is fine, and is the common case.

It speaks only for a plain `if`/`then`/`else`. A chain with an `elif` has more than two ways through
it and no single swap that puts the short one first. It stays quiet on a conditional directive inside
the expression, and offers no fix, because rewriting a condition is a thing to read before doing.

## FANTOMAS-KEEPINDENT-001

Once the short branch is first, let the last branch keep the indentation of the expression:

```fsharp
match localToolsListResult with
| Ok(CompatibleTool version) -> Ok(FantomasToolFound(version, FantomasToolStartInfo.LocalTool workingDir))
| Error err -> Error(FantomasToolError.DotNetListError err)
| Ok _localToolListResult ->

let globalToolsListResult = runToolListCmd workingDir true

match globalToolsListResult with
| Ok(CompatibleTool version) -> Ok(FantomasToolFound(version, FantomasToolStartInfo.GlobalTool))
| Error err -> Error(FantomasToolError.DotNetListError err)
| Ok _nonCompatibleGlobalVersion ->

let fantomasOnPathVersion = fantomasVersionOnPath ()
```

This is the other half of what `FANTOMAS-ARMORDER-001` starts, and the reason that rule cares about
order in the first place. The short arms say what the rest of the expression is not about and then
get out of the way, and what is left is the one path that continues, written at the indentation it
started at. Three lookups falling through to each other cost no indentation at all, where nesting
them would have cost twelve columns by the third.

`fsharp_experimental_keep_indent_in_branch`, which the repository's `.editorconfig` turns on, is what
holds the body there, and it only holds a body that was already written that way: it will not
de-indent for you, and it will re-indent one written that way where the setting is off. So the whole
style depends on somebody writing it, which is what this rule is for. The `.editorconfig` covers
`src` and `analyzers`, which together are everything the pipelines analyze, so a finding is never one
the formatter will undo.

The analyzer is narrower than the rule, because moving a body left can change what runs:

- Only the **last** arm, since that is the only one `CodePrinter` offers the choice to, and since a
  following arm of the same match would be the first thing a de-indented body swallowed.
- Only a body that is a **block**: another `match`, an `if`, or a sequence of bindings and
  statements. That is where the columns are saved again by everything inside. A single application
  or pipeline has nothing under it to save them for and reads oddly under the blank line the setting
  writes.
- Only a body **already on a line of its own** and spanning more than one line. A body that fits
  beside its arrow gets pulled up next to it and never reaches the branch that would keep it.
- Only where **every other arm is a one liner**. That is the early return shape, and it is what
  makes the de-indent mean anything: the arms that decline say so and get out of the way, and what
  is left is the one path that carries on. A match whose other arms are blocks too is not that
  shape, and de-indenting the last of them alone puts arms of the same kind at two different
  indentations and says the last is special when it is not.
- Only where **nothing follows the match in that column**. This is the one way the reshape changes
  meaning. De-indenting moves the offside line of the body out to the bar, so the first thing after
  the match that starts in that column or further right stops following the match and starts
  belonging to its last arm. Anything further left ends the arm exactly as it ended the match.

  The rule answers this by reading the source: it takes the first line after the one the match ends
  on that has any content, and compares its indentation. Whatever shares the match's last line moves
  with the body and keeps its place, which is how a closing bracket stays out of it.

  That is deliberately a question about text rather than about the tree, and it is the third
  attempt. Collecting `SynExpr.Sequential` pairs missed the `json.WriteEndObject()` after the match
  in `writeDoctorFile`, so it ran for one case out of three and every doctor report came out as
  truncated JSON. Flattening those sequences properly then missed the `|> genNode attr` under the
  match in `genAttributesCore`, which applied to the whole match and would have applied to one arm,
  so everything reached through the other arm lost its trivia and the compiler-define tests failed.
  Both were shapes to enumerate and there was always going to be another one. The text has none, and
  it costs a comment its place at worst: a comment under the match counts as content, so the rule
  stays quiet rather than move it into the arm.

It stays quiet on a `when` guard, because a multiline guard takes a path in `CodePrinter` that
indents the body whatever column it is in, and whether a guard prints multiline is a page width
question rather than a tree one. It stays quiet on a conditional directive inside the match. And it
offers no fix, because re-indenting a block means leaving the multiline strings inside it exactly
where they are, which is not a thing to do blind, least of all in `Fantomas.Core.Tests`.

`match`, `match!` and `function` all reach the same clause printer, so all three are covered. So is
the final `else` of an `if`, which reaches `genKeepIdentIfThenElse` rather than
`genKeepIdentMatchClause` and is a shade more permissive: it accepts the body in the column of the
`else` or of the `if`, where a match arm has only the `|` to match. Fantomas prints both in the same
column, so the rule aims at the `else` and one target is enough. An `elif` chain is printed flat and
offers the choice to its last `else` alone, so the chain is walked to reach it and every `then` above
has to be a one liner like any other branch.

The two halves compose, and the composition is the point. `FANTOMAS-BRANCHORDER-001` and
`FANTOMAS-ARMORDER-001` put the short branches first, which leaves the one that carries on last,
which is where this rule can reach it. Fixing them in that order is worth doing, because a swap
creates candidates here that were not there before.

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

A tuple parameter counts as annotated when every element of it is, so `(a: int, b: string)` is
accepted and does not have to be rewritten as `((a, b): int * string)`. Both state the type of
the parameter, and the first is the one people write.

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

A declaration the signature file does not carry is left alone, doc comment and all. There is no
second copy to keep in step, so there is nothing for the rule to be about: write `///` on a private
helper in a file that has an `.fsi` and nothing complains.

The rule used to report every `///` in a file that had a signature file, because it could not tell
which of them were duplicated, and the answer there was to write `//` instead. That is no longer
the convention, and the `//` comments left over from it are not worth converting on sight.

What it asks the compiler is `FSharpSymbol.SignatureLocation`, and that is worth knowing before
using it elsewhere: it is not the yes or no it reads as. For a symbol the signature does not carry it
falls back to the declaration itself, so it is `Some` for every symbol and `IsSome` answers nothing.
What separates the two is which file it points at — into the `.fsi` for a symbol the signature
declares, back at the `.fs` for one it does not.

## FANTOMAS-OPENS-001

Remove an `open` that nothing in the file resolves through. It is a name the reader has to hold
while reading everything below it, and it says the file depends on something it does not.

The compiler answers this rather than the rule: `FSharp.Compiler.EditorServices.UnusedOpens`, which
is the same call FsAutoComplete makes for the diagnostic it raises as `FSAC0001`. It walks every
symbol use of the file and keeps the opens that were needed to write a name the way it is written,
so an open kept only to shorten a type annotation counts as used. That is a question about the typed
tree, which is why the rule reads `ctx.CheckFileResults` and is quiet in the editor without them.

**The reported range is the module identifier, not the declaration.** `open System` reports
`System` alone, columns 5 to 11. What has to go is the whole line, including its linebreak, so no
blank line is left where the declaration was. There is no fix attached, for the reason every other
rule here has none, and here it costs the least: deleting a line needs no re-indentation and cannot
glue two tokens together.

It runs on signature files as well as implementation files. An `open` in an `.fsi` that no `val` or
type in it resolves through is unused in exactly the same sense, and the compiler answers it the
same way.

Two things it does not see, both inherited from the compiler's own detection: an `open` that only
brings an operator into scope, and one that only brings a type extension into scope. FsAutoComplete
ships this analyzer disabled by default where it ships the parentheses one enabled, which is the
clearest available signal about how far to trust it. Nothing in this repository triggers either gap
today: every finding of the first full run was real, and the whole solution still built with all
eight of them removed. But a finding that looks wrong is worth checking against the build before
acting on it, because deleting a needed `open` breaks the build rather than failing quietly.

Generated sources are excluded rather than reported. `scripts/BuildAnalyzers.fsx` passes
`**/*.AssemblyInfo.fs` to `--exclude-files`: MSBuild writes one per project under `obj`, opening
`System` and `System.Reflection` and then writing every attribute out fully qualified, so the rule
has two true things to say about each of them and nowhere to say them. Note that `--exclude-files`
and `--include-files` are mutually exclusive in the tool, which drops the former with a warning when
both are given. `AnalyzeChanged` therefore ignores the exclusion, and does not need it: it includes
the files the working tree changed, and a generated file under `obj` is never one of them.

## Severity and scope

Severity and scope are separate levers. Severity decides whether the run fails: the tool exits
non-zero on any finding at error severity, so the two error rules fail `Analyze` and fail CI. Scope
decides who has to look: a rule excluded from `Analyze` is absent from CI and from GitHub code
scanning whatever its severity, and still reports locally.

`FANTOMAS-ANNOTATE-001` is excluded from `Analyze` because it reports on debt that predates it.
`AnalyzeChanged` runs it, and narrows it further to the lines `git diff` says you touched: a file is
a much coarser scope than that rule asks for, and one line changed in a file of several thousand
otherwise surfaces every unannotated binding in it. Every other rule reports wherever it fires in a
file you edited, because a finding from one of those is worth seeing.

`FANTOMAS-KEEPINDENT-001` and `FANTOMAS-OPENS-001` each arrived with debt of their own and are both
in both pipelines, because that debt was cleared in the change that added them. The unused-open
count was the thing to measure before deciding: eight findings in hand-written source, all of them
real, which is small enough to fix outright rather than carry in the advisory lists. The full run therefore has nothing old to report, and
anything it does report is something the change in front of you introduced, which is the state to
keep it in: a new case is cheap to fix while you are writing it and becomes a code scanning alert if
you do not.

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
Ionide as you type. Four of the five rules read only `ctx.FileName`, `ctx.ProjectOptions.SourceFiles`
and `ctx.ParseFileResults.ParseTree`, all of which `EditorContext` carries as well as `CliContext`
does.

`FANTOMAS-XMLDOC-001` and `FANTOMAS-OPENS-001` also read `ctx.CheckFileResults`, because the untyped
tree can say neither whether the signature file declares the same binding nor what a name resolved
through. That does not cost the editor registration:
`EditorContext` carries check results too, as an option rather than outright, so the editor analyzer
matches on it and says nothing when they are absent. A rule that needs the typed tree is fine; one
that cannot answer without it has to be quiet in the editor rather than wrong there.

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
what the two rules keyed on the signature file need, and `analyzeSignature` builds the same pair and
analyzes the `.fsi` of it instead, which is how `FANTOMAS-OPENS-001` is held to what it does there. A snippet has to begin with a module
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

None of this is currently load bearing for the four rules that read only the untyped tree. Parsing
every file of `Fantomas.Core` under the default options, both define sets, `--strict-indentation+`
and `--langversion:preview` produces identical findings from them, which is what you would expect.
`FANTOMAS-XMLDOC-001` and `FANTOMAS-OPENS-001` read the typed tree and so do depend on the project
options resolving, which is another reason the fixture checks they came back non-empty. The defines are the only option that could change a verdict, since they decide which branch of
an `#if` reaches the tree, and `DEBUG` in `Selection.fs` is the only one in real source anywhere.
