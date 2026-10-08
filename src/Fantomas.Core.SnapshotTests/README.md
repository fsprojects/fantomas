# Fantomas.Core.SnapshotTests

Formatting tests as files. Every F# file under `cases/` is a case, its gold files beside it hold
what formatting it produces, and one test per case compares the two.

## Running

Every case is a test named after its path, `case("oak/TypeDefn/Union/cases-on-their-own-lines.fs")`,
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
dotnet fsi scripts/format.fsx <file>    the result, and every problem this project would fail the case on (exit code 1)
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
  `---*)`, holding editorconfig properties, one `key = value` per line. A line starting with `#`
  is the description, and one starting with `;` a comment, as in an `.editorconfig`. It is read by
  the code the tool uses, and anything that is no setting, or a value Fantomas cannot act on, fails
  the case. So does a setting set twice, and so do the values the editorconfig spec reserves,
  `unset`, `indent_size = tab` and `max_line_length = off`: the tool keeps its default for them,
  which in a case would test something other than it says. It is stripped before formatting, so
  nothing in it is parsed as F#.
- **Kind of file.** `name.fs` is an implementation file, `name.fsi` a signature file. A signature
  case needs no module or namespace header unless the parser asks for one. See "Signature files"
  for when a `.fsi` case is worth having.
- **Name.** Lower case words joined by dashes, with the issue number first when the case comes
  from an issue: `1483-case-behind-a-define.fs`. Only use the number of an issue the case comes
  from; do not guess one. Keep it short: the longest path in the repository is 190 characters, and
  a Windows checkout a few folders deep reaches the 260 Windows allows unless git sets
  `core.longpaths`. A gold per define combination makes the name longer still.
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
  comment or directive, trailing whitespace, different with `\r\n` line endings, or disagreeing
  with production) is never written as a gold, not even by `UpdateSnapshots`. Neither is the gold
  of a case in the wrong folder (see "Where a case goes").
- **Line endings.** A gold differing from the result only in its line endings fails like any
  other, and the diff shows every carriage return as `\r`.
- **Strays.** Every file under `cases/` is a case, a gold or `.actual` of one, or a `README.md`.
  Anything else fails the run, except a hidden file such as the `.DS_Store` macOS leaves behind.
  An `.actual` whose case was renamed or deleted is deleted rather than reported: git ignores it,
  so nothing else would show it.

## Ignored cases

A case for a bug that is not fixed yet is `name.ignore.fs`. Its golds hold what formatting should
give, written by hand: `name.gold.fs`, the same name it will have once the `.ignore` goes.

- **Reason.** The front matter's `#` description says why it is ignored, an issue link at best. A
  run lists the case as skipped with that reason, and fails a case that gives none. An issue opened
  for the bug later goes in the reason too, so a search for its number finds the case.
- **Comparing.** Only the golds the case has are compared, and nothing writes them, not even
  `UpdateSnapshots`. What it gives today is written to the `.actual` beside a gold it does not
  match. A gold whose line endings formatting never gives, `\r\n` where the case formats with
  `\n` say, fails the case: an editor that saved it its own way would otherwise keep the case
  ignored after its bug is fixed.
- **Fixed.** Once it gives its golds and passes every check, it fails with "rename it to
  `name.fs`". So an ignored case cannot stay ignored after its bug is gone. The reason goes with
  the `.ignore`: write a description of what the case shows in its place, or none when the name
  says it.
- **Folder.** What its folder asks of the input still holds: the node the folder names, and the
  setting it names, set to its value. So does what is true of its golds whatever the result: none
  beside a case under `negative/`, and none for a define combination the input does not have. The
  node and the define combinations are only known once the case formats, so a case that throws is
  skipped without those two. The result is what is known to be wrong, so the checks of the result
  wait until the case passes. A case that throws is skipped with what it throws after its reason.
- **Both.** `name.fs` and `name.ignore.fs` cannot sit side by side: both fail.

## What every case checks

- the result is valid F#, under every define combination;
- every comment of the input is in the result, and as many of them;
- every conditional directive (`#if`, `#elif`, `#else`, `#endif`) and warn directive (`#nowarn`, `#warnon`)
  of the input is in the result, with the same text and in the same order. Both are trivia, like
  comments. Blank lines
  are left out: formatting adds and removes them on purpose. Comments and directives are compared
  under each define combination the input has, since what sits in a branch is only there under the
  defines that keep it;
- the result is idempotent, merged and per define combination;
- every node's `Children` are in source order;
- every node lies within its parent's range;
- no line ends in whitespace, except one that ends inside a string or a comment spanning several
  lines, where the whitespace is content;
- with `\r\n` line endings in and `end_of_line = crlf`, the result is the same with `\r\n` line
  endings, or for a case that sets `end_of_line = crlf` the same result. That is what Windows users
  get. A case at `end_of_line = cr` is left out;
- every comment and directive the parser recorded is attached to a node of the Oak, under each
  define combination.

The first three, and merged idempotency, are what `formatDocument` checks when asked for every
check (`Validations.All`). A comment is compared by its text and its kind: a line comment may move
between a line of its own and the end of a line of code, and a block comment may not. A format run
and the daemon ask only for the first (`Validations.Parse`), and search the result for the text of
every comment of the source instead, in the order of the source (`Validations.CommentSearch`).
`fantomas doctor` asks for every check, like the cases. Every case checks that this search finds
them all, since a comment it misses is a file users cannot format.

## Where a case goes

```
cases/scenarios/<old test file>/[negative/]                        scenarios/CommentTests/
cases/oak/<Union>/<Case>/[trivia/][negative/]                      oak/TypeDefn/Union/trivia/negative/
cases/oak/<Node>/[trivia/][negative/]                              oak/UnionCase/
cases/settings/<key>/[<value>/]<Union>/<Case>/[trivia/]            settings/fsharp_bar_before_discriminated_union_declaration/true/TypeDefn/Union/
cases/settings/<key>/[<value>/]<Node>/[trivia/]                    settings/fsharp_max_function_binding_width/Binding/
cases/settings/<key>/[<value>/]negative/<Union>/<Case>/[trivia/]   settings/fsharp_bar_before_discriminated_union_declaration/true/negative/ModuleDecl/Exception/
cases/settings/<key>/[<value>/]negative/<Node>/[trivia/]
```

`negative/` comes last below a node, and right after the value below a setting. A case under a
setting is in the folder of a node as well.

Every union case and node class of the Oak has its folder under `oak/`, and every setting its folder
under `settings/`, each with at least one case to start from;
[`cases/oak/README.md`](cases/oak/README.md) names the few nodes no case can hold. A test holds every
folder to it, so a node or a setting added to Fantomas needs its first case with it. `scenarios/` holds
the tests that were in `Fantomas.Core.Tests` (see "Scenarios" below). A new case goes under `oak/` or
`settings/`:

1. **A setting.** If the point of a case is what a setting does, it goes under `settings/<key>/`.
   A switch has a folder for each side, `true/` and `false/`, and a setting with named values one
   for each value, `aligned/`, `cramped/` and `stroustrup/`: the default has its folder too, and a
   case of the setting is in one of them. A number has no value folder, and a case of it sets a
   value other than the default. A case a setting must leave alone goes in `negative/` below the
   value: an exception, say, which never gets the bar
   `fsharp_bar_before_discriminated_union_declaration` puts before a single union case. Such a case
   has no gold: its input is already formatted, and formatting it at every value must give it back
   unchanged.
2. **A node.** Otherwise the case goes in the folder of the node it is about, at the default
   settings. A smaller `max_line_length` that only keeps the input short does not make it a
   settings case.
3. **Trivia.** A case about comments, blank lines or directives goes in the `trivia/` folder of the
   node it is about. Which node `Trivia.fs` attaches them to is not checked: that is how it works
   today, and it can change without the formatting changing. `dotnet fsi scripts/trivia.fsx <file>`
   shows where they land, when that helps to understand a result.
4. **Several nodes.** A relation between nodes belongs to the parent.
5. **Left alone.** A case formatting must leave as it is goes in `negative/`, last below its node
   folder: `oak/ModuleDecl/ModuleAbbrev/trivia/negative/line-comment-after-the-alias.fs`, a comment
   that must stay where it is. It has no gold, and its result must be its input. One with `#if` keeps its
   per-define golds, since what each combination printed is not its input. A negative case is as
   much a test as any other: while fixing a bug it is often the one that says what must not change.

The path is checked:
- under `scenarios/`, only `negative/`: the folders name the file the tests came from, not a node;
- a case under `settings/<key>/` must set `<key>`, to its value folder when there is one, and to
  other than its default when there is none;
- `<key>` at another value must change the result: the default, for a case at another value, and
  some other value, for a case at the default. Under `negative/` no other value may change the
  input;
- a case must contain the node its folder names: the node class for `oak/<Node>/`, and the union
  case itself for `oak/<Union>/<Case>/`, held by some node of the Oak. A node class some other node
  holds as well does not count: `(fun x -> x)` is no `Expr.Lambda`, it is an `Expr.ParenLambda`;
- a case under `negative/` must come back unchanged, and any other case must not, nor may its
  result differ from the input only at its end, a final newline added say. Such a case belongs in
  `negative/` with that ending. What formatting does to the end of a file is the point of the cases
  under `settings/insert_final_newline/`, and of a case that sets it to other than its default, so
  those are left out.

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

`dotnet fsi build.fsx -- -p CoverageOak` measures `SyntaxOak.fs` alone. `syntaxoak-coverage.txt`
names the classes some case reaches in full, then lists, by class, every line no case reaches.
Every node class constructor and every arm of a union's `Node` member is a node or a union case
that some case must contain.

## Scenarios

`cases/scenarios/` holds the tests `Fantomas.Core.Tests` had, one folder per test file. They are
inputs shaped by what users ran into, often with several settings at once because the settings
meet, and none is about one node or one setting. That is why they are not split up into `oak/` and
`settings/`: a case there would lose the combination it was written for. A name that starts with a
number is the issue the case comes from.

A new case goes in `scenarios/` only when it is such a combination: settings that interact, or a
realistic input that no single node folder can hold. When a scenario breaks, the smallest case that
shows why goes under `oak/` or `settings/`, and the scenario stays as it is.

[`cases/scenarios/README.md`](cases/scenarios/README.md) has where they come from, how they were
converted, and how to find their history. A `README.md` in one of their folders holds what the old
test file said around its tests.
