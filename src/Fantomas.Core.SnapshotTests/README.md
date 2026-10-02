# Fantomas.Core.SnapshotTests

Formatting tests as files. Every F# file under `cases/` is a case, its gold files beside it hold
what formatting it produces, and one test per case compares the two.

## Running

Every case is a test named after its path, `case("oak/TypeDefn/Union/single-case-with-fields.fs")`,
so a test explorer lists them one by one, and `--filter` picks them by any part of the path:

```
dotnet test src/Fantomas.Core.SnapshotTests                                      every case
dotnet test src/Fantomas.Core.SnapshotTests --filter "Name~1483"                  one case
dotnet test src/Fantomas.Core.SnapshotTests --filter "Name~oak/TypeDefn/Union/"   a folder, trivia/ included
dotnet test src/Fantomas.Core.SnapshotTests --list-tests                          every case's name
```

A failing case prints a line diff against its gold and writes what came out to the `.actual` file
beside it.

To look at an input without making it a case, use the scripts in `scripts/`. They read a case's
front matter as its settings, so they take a case file as it is:

```
dotnet fsi scripts/format.fsx <file>    the result, and every problem this project would fail the case on
dotnet fsi scripts/trivia.fsx <file>    where each piece of trivia landed: node, token, side and kind
dotnet fsi scripts/oak.fsx <file>       the whole Oak
```

They reference this project as built, so its checks are the ones they run: build it in debug first.
What they do not check is the folder: write the case and run it filtered for that.

## Accepting a change

```
FANTOMAS_UPDATE_SNAPSHOTS=1 dotnet test src/Fantomas.Core.SnapshotTests --filter "Name~1483"
dotnet fsi build.fsx -- -p UpdateSnapshots
```

- **One case.** The first line rewrites the golds of the cases the filter picks. In PowerShell, set
  `$env:FANTOMAS_UPDATE_SNAPSHOTS=1` first. Renaming the `.actual` over the gold does the same.
- **Everything.** The second line rewrites every gold.
- **Reviewing.** `git diff cases/` shows what was accepted.

## Writing a case

1. Decide what the case shows. Either formatting changes the input, and the gold shows how, or
   formatting must leave the input as it is, and the case goes in a `negative/` folder, where it is
   its own gold. A case outside `negative/` whose result is its input fails: change the input so the
   result earns its gold.
2. Put the input in the right folder (see below), named after what it shows:
   `cases/oak/TypeDefn/Union/single-case-with-members.fs`. Add a `#` description when the name
   does not say the point on its own.
3. Run it with `FANTOMAS_UPDATE_SNAPSHOTS=1` and a filter on its name. That writes its golds, or
   fails without writing any when the result is broken or the case is in the wrong folder.
4. Read the gold. It is the formatting the case now pins down, so check it is what you expected.
   The checks do not see a comment that moved, or code that was lost while the result stays valid
   F#: only reading does.
5. Run the reports to see what the folder still misses.

## A case

```fsharp
(*---
# What this case shows. `#` lines are the description.
fsharp_bar_before_discriminated_union_declaration = true
---*)
type A = A of int
```

- **Front matter.** Optional. A block comment that starts on line 1 with `(*---` and ends with
  `---*)`, holding editorconfig properties. It is read by the code the tool uses, and anything
  that is no setting, or a value Fantomas cannot act on, fails the case. It is stripped before
  formatting. F# lexes strings and nested comments inside a block comment, so keep any `"` in a
  description balanced, and do not write `(*` in one.
- **Kind of file.** `name.fs` is an implementation file, `name.fsi` a signature file. A signature
  case needs no module or namespace header unless the parser asks for one. See "Signature files"
  for when a `.fsi` case is worth having.
- **Name.** Lower case words joined by dashes, with the issue number first when the case comes
  from an issue: `1483-case-behind-a-define.fs`. Only use a number the old test or the issue names;
  do not guess one.
- **Line endings.** Every input is read with `\n` line endings, and `end_of_line` is `lf` unless
  the front matter sets it. `.gitattributes` keeps `cases/` byte for byte.

## Golds

- `name.gold.fs` holds the formatted result. A case under `negative/` has none: it is its own gold.
- **Defines.** A case with `#if` also gets one gold per define combination, `name.no-defines.gold.fs`,
  `name.DEBUG.gold.fs`, `name.DEBUG+TRACE.gold.fs`, and `name.gold.fs` holds the merged result
  users get. The per-define golds are what each combination printed before the merge, which is why
  a directive can be indented in them. An old test that formatted one combination merged its result
  with itself first, which moved its directives to column 0, so a per-define gold need not match its
  expected output there.
- **Mismatch.** The test fails with a line diff and writes `name.actual.fs` beside the gold. Rename
  it over the gold to accept it, or run `dotnet fsi build.fsx -- -p UpdateSnapshots` to accept every
  change.
- **Stale golds.** A gold whose case is gone, or one for a define combination the case no longer
  has, fails the run.
- **Broken results.** A result that fails one of the checks below (invalid, not idempotent, a lost
  comment, trailing whitespace, or disagreeing with production) is never written as a gold, not
  even by `UpdateSnapshots`.

## Ignored cases

A case for a bug that is not fixed yet is `name.ignore.fs`. Its golds hold what formatting should
give, written by hand: `name.gold.fs`, the same name it will have once the `.ignore` goes.

- **Reason.** The front matter's `#` description says why it is ignored, an issue link at best. A
  run lists the case as skipped with that reason, and fails a case that gives none.
- **Comparing.** Only the golds the case has are compared, and nothing writes them, not even
  `UpdateSnapshots`. What it gives today is written to the `.actual` beside a gold it misses.
- **Fixed.** Once it gives its golds and passes every check, it fails with "rename it to
  `name.fs`". So an ignored case cannot stay ignored after its bug is gone.
- **Both.** `name.fs` and `name.ignore.fs` cannot sit side by side.

## What every case checks

- the result is valid F#, under every define combination;
- every comment of the input is in the result, and as many of them;
- the result has as many conditional directives (`#if`, `#else`, `#endif`) and warn directives
  (`#nowarn`, `#warnon`) as the input. Both are trivia, like comments. Blank lines are left out:
  formatting adds and removes them on purpose;
- the result is idempotent, merged and per define combination;
- `Node.Children` lists every node in source order;
- no line ends in whitespace;
- the harness, which formats each define combination itself, agrees with `formatDocumentWith`,
  which is what users run.

## Where a case goes

```
cases/ported/<old test file>/[negative/]                   ported/CommentTests/, made by scripts/convert.fsx
cases/oak/<Union>/<Case>/[trivia/][negative/]              oak/TypeDefn/Union/trivia/negative/
cases/oak/<Node>/[trivia/][negative/]                      oak/UnionCase/
cases/settings/<key>/[<value>/]<Union>/<Case>/[trivia/]    settings/fsharp_bar_before_discriminated_union_declaration/TypeDefn/Union/
cases/settings/<key>/[<value>/]negative/<Union>/<Case>/    settings/fsharp_bar_before_discriminated_union_declaration/negative/ModuleDecl/Exception/
```

`ported/` holds the old tests, converted without judgement (see "The ported tests" below). A case
written by hand goes under `oak/` or `settings/`:

1. **A setting.** If the point of a case is what a setting does, it goes under `settings/<key>/`,
   setting a value other than the default. The value folder is only there for settings with named
   values, such as `stroustrup`. A case a setting must leave alone goes in `negative/` below the
   setting: an exception, say, which never gets the bar
   `fsharp_bar_before_discriminated_union_declaration` puts before a single union case. Such a case
   has no gold: its input is already formatted, and formatting it with the setting and with the
   setting at its default must both give it back unchanged.
2. **A node.** Otherwise the case goes in the folder of the node it is about, at the default
   settings. A smaller `max_line_length` that only keeps the input short does not make it a
   settings case.
3. **Trivia.** A case about comments, blank lines or directives goes in the `trivia/` folder of the
   node it is about. Which node `Trivia.fs` attaches them to is not checked: that is how it works
   today, and it can change without the formatting changing. `dotnet fsi scripts/trivia.fsx <file>`
   shows where they land, when that helps to understand a result.
4. **Several nodes.** A relation between nodes belongs to the parent.
5. **Left alone.** A case formatting must leave as it is goes in `negative/`, last below its node
   folder: `oak/UnionCase/trivia/negative/2606-comment-after-the-last-case.fs`, a comment that must
   stay where it is. It has no gold, and its result must be its input. One with `#if` keeps its
   per-define golds, since what each combination printed is not its input. A negative case is as
   much a test as any other: while fixing a bug it is often the one that says what must not change.

The path is checked:
- under `ported/`, only `negative/`: the folders name the file the tests came from, not a node;
- a case under `settings/<key>/` must set `<key>`, to its value folder when there is one;
- resetting `<key>` to its default must change the result, except under `negative/`, where
  neither the setting nor its default may change the input;
- a case must contain the node its folder names;
- a case under `negative/` must come back unchanged, and any other case must not.

A union case whose node class other cases carry too, such as a bare `SingleTextNode`, cannot be
told apart in the Oak, so its folder's node check is skipped.

## Signature files

Signature files build their own declarations: module and namespace headers, nested modules,
module abbreviations, `val`s, exceptions, type definitions, and every member, as a member
signature. Expressions and patterns do not occur in them. So a `.fsi` case belongs in the folders
of those declarations, where the signature path can print differently. Elsewhere it adds nothing.
A `.fs` and a `.fsi` case beside each other are two cases: their inputs say what each kind of file
would say, and need not match. Some syntax does not exist in a signature file at all, `module rec`
and `extern` among it: `dotnet fsi scripts/format.fsx --signature <file>` says so before a case is
written.

## Reports

`dotnet fsi build.fsx -- -p SnapshotReports` writes two reports over all cases into `reports/`,
which git ignores:
- `reports/shapes.md`: for every node class, whether some case has each optional part and some
  case leaves it out, and whether some case has none, one and several of each list of parts;
- `reports/trivia.md`: where trivia lands on every node class.

They are there to read while writing cases for a folder. They are no golds and no test: they
would change with every case. The pipeline sets
`FANTOMAS_SNAPSHOT_REPORTS=1`, and a test run with that set writes them once it finishes. A shape
under Missing is either a case still to write or one the parser cannot produce, and the person
writing the cases judges which. One the parser cannot produce needs no record anywhere.

`dotnet fsi build.fsx -- -p CoverageReach` measures what each old test and each case reaches in all
of Fantomas.Core, one at a time, and writes to `artifacts/coverage/`: `reach.tsv`, every test and
case with the points it reached, and `parity.md`, what the old tests reach that no case does, by
source line, and which of the old tests that stay reach it. A point is a line or a branch, as AltCover
counts them. Module initialisers run before anything is measured, since whatever ran first would
otherwise get them to itself.

`dotnet fsi build.fsx -- -p CoverageOak` measures `SyntaxOak.fs` alone. `syntaxoak-coverage.txt`
names the classes some case reaches in full, then lists, by class, every line no case reaches.
Every node class constructor and every arm of a union's `Node` member is a node or a union case
that some case must contain.

## The ported tests

`cases/ported/` is `Fantomas.Core.Tests` converted by `scripts/convert.fsx`, one folder per test
file. Nothing in it was judged or rewritten:

- **What becomes a case.** A test that formats a string with a config and compares the result with
  an expected string, through `formatSourceString`, `formatSignatureString`,
  `formatSourceStringWithDefines` or `CodeFormatter.FormatDocumentAsync`, optionally with
  `prepend newline` in between. The strings may be literals, `let`s, interpolated strings, or a
  helper's result formatted again. A test of several such steps, or one that calls a function of its
  file which is, becomes a case per step; a `[<TestCase>]` or `[<TestCaseSource>]` test, a case per
  argument.
- **The case.** Its input as written, its config as front matter, and the harness result as its
  golds. The newline a triple quoted input starts with is dropped when the result stays the same
  without it. The tests of one file that format the same input with the same settings are one case.
- **The proof.** Before anything is written, every test's expected output is compared with the
  harness result: the merged gold, or, for `formatSourceStringWithDefines`, the result for its
  defines merged with itself, as that helper does. A test whose result is its input becomes a
  negative case. A case's name comes from its first test's name, its issue number first.
- **Ignored tests.** An `[<Ignore>]` test is checked like any other and converted once it passes.
  Until then it becomes an ignored case, its reason as the description and its expected output as
  the gold. One that formats a single define combination cannot: no gold holds that output.
- **The rest.** A test that does not fit stays where it is and is listed with the reason: the unit
  test files, `formatAST` tests (which format a tree without its source, so without trivia), tests
  that format a generated input to check it does not overflow the stack, and one that checks an
  exception.

`porting-ledger.tsv` is written by the converter alongside: one row per old test, with the case it
became or why there is none.

```
dotnet fsi scripts/convert.fsx                 dry run: what each test would become, in counts
dotnet fsi scripts/convert.fsx -- --write      write cases/ported/ and the ledger again
dotnet fsi scripts/convert.fsx -- --check      fail when cases/ported/ or the ledger differ from the old tests
```

While both suites exist, the old tests are the source of `ported/`: a change in formatting is made to
the old test, and `--write` brings the case along. Do not edit `ported/` by hand, `--check` says
when it no longer matches.
