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

1. Put the input in the right folder (see below), named after what it shows:
   `cases/oak/TypeDefn/Union/single-case-with-members.fs`.
2. Run it with `FANTOMAS_UPDATE_SNAPSHOTS=1` and a filter on its name. That writes its golds, or
   fails without writing any when the result is broken or the case is in the wrong folder.
3. Read the gold. It is the formatting the case now pins down, so check it is what you expected.
4. Run the reports to see what the folder still misses.

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
  formatting. F# lexes strings inside a block comment, so keep any `"` in a description balanced.
- **Kind of file.** `name.fs` is an implementation file, `name.fsi` a signature file.
- **Name.** Lower case words joined by dashes, with the issue number first when the case comes
  from an issue: `1483-case-behind-a-define.fs`.
- **Line endings.** Every input is read with `\n` line endings, and `end_of_line` is `lf` unless
  the front matter sets it. `.gitattributes` keeps `cases/` byte for byte.

## Golds

- `name.gold.fs` holds the formatted result.
- **Defines.** A case with `#if` also gets one gold per define combination, `name.no-defines.gold.fs`,
  `name.DEBUG.gold.fs`, `name.DEBUG+TRACE.gold.fs`, and `name.gold.fs` holds the merged result
  users get. The per-define golds are what each combination printed before the merge, which is why
  a directive can be indented in them.
- **Mismatch.** The test fails with a line diff and writes `name.actual.fs` beside the gold. Rename
  it over the gold to accept it, or run `dotnet fsi build.fsx -- -p UpdateSnapshots` to accept every
  change.
- **Stale golds.** A gold whose case is gone, or one for a define combination the case no longer
  has, fails the run.
- **Broken results.** A result that fails one of the checks below (invalid, not idempotent, a lost
  comment, trailing whitespace, or disagreeing with production) is never written as a gold, not
  even by `UpdateSnapshots`.

## What every case checks

- the result is valid F#, under every define combination;
- every comment of the input is in the result, and as many of them;
- the result is idempotent, merged and per define combination;
- `Node.Children` lists every node in source order;
- no line ends in whitespace;
- the harness, which formats each define combination itself, agrees with `formatDocumentWith`,
  which is what users run.

## Where a case goes

```
cases/oak/<Union>/<Case>/[trivia/]                         oak/TypeDefn/Union/
cases/oak/<Node>/[trivia/]                                 oak/UnionCase/
cases/settings/<key>/[<value>/]<Union>/<Case>/[trivia/]    settings/fsharp_bar_before_discriminated_union_declaration/TypeDefn/Union/
```

1. **A setting.** If the point of a case is what a setting does, it goes under `settings/<key>/`,
   setting a value other than the default. The value folder is only there for settings with named
   values, such as `stroustrup`.
2. **A node.** Otherwise the case goes in the folder of the node it is about, at the default
   settings. A smaller `max_line_length` that only keeps the input short does not make it a
   settings case.
3. **Trivia.** Comments, blank lines and directives go in the `trivia/` folder of the node they are
   attached to, which is the deepest node at that spot. A comment at the end of a union case line
   attaches to the last token of the field's type, so it belongs with that type, not with the case.
   `dotnet fsi scripts/oak.fsx <file>` shows where trivia lands.
4. **Several nodes.** A relation between nodes belongs to the parent.

The path is checked:
- a case under `settings/<key>/` must set `<key>`, to its value folder when there is one;
- resetting `<key>` to its default must change the result;
- a case must contain the node its folder names;
- a case in `trivia/` must attach trivia to that node or one of its direct children.

A union case whose node class other cases carry too, such as a bare `SingleTextNode`, cannot be
told apart in the Oak, so its folder's node check is skipped.

## Reports

`dotnet fsi build.fsx -- -p SnapshotReports` writes two reports over all cases, into `reports/`,
which git ignores:
- `reports/shapes.md`: for every node class, whether some case has each optional part and some
  case leaves it out, and whether some case has none, one and several of each list of parts;
- `reports/trivia.md`: where trivia lands on every node class.

They are there to read while porting a folder. They are no golds and no test runs them: they would
change with every case, and folders ported in parallel would fight over them. A shape under Missing
is either a case still to write or one the parser cannot produce, and the person porting the folder
judges which.

`dotnet fsi build.fsx -- -p CoverageOak` measures `SyntaxOak.fs` alone and writes what no case
reaches to `syntaxoak-coverage.txt`. Every node class constructor and every arm of a union's
`Node` member is a node or a union case that some case must contain.

## The porting ledger

`porting-ledger.tsv` has one row per test in `Fantomas.Core.Tests`, with the old test's input
config and the Oak node classes its input contains. `status` is:

| Status | Meaning |
|---|---|
| `todo` | not ported yet; `targets` names the folder it belongs to once someone looked at it |
| `ported` | became the cases in `targets` |
| `merged` | covered by the case in `targets`, for the reason given |
| `dropped` | no case, for the reason given |
| `unit` | stays an F# unit test |

```
dotnet fsi scripts/ledger.fsx                      regenerate, keeping status, targets and reason
dotnet fsi scripts/ledger.fsx -- --contains A,B    the old tests whose input has node A or B
dotnet fsi scripts/ledger.fsx -- --resolved        what removing the old tests would delete
```
