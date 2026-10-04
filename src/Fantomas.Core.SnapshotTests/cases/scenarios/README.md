# Scenarios

Every case here was a test in `src/Fantomas.Core.Tests` before #3512 replaced those tests with snapshot
cases. Each folder is one test file, named after it, and `Stroustrup/` is the subfolder of the same name.
Most tests were written while fixing an issue: a name that starts with a number is that issue, and
`--filter "Name~3484"` finds its case.

They are inputs shaped by what users ran into, often with several settings at once because the settings
meet. That is why they stay together instead of being split up into `oak/` and `settings/`: a case there
is about one node or one setting, and these would lose the combination they were written for. A new case
belongs here when it is such a combination too.

## How a test became a case

A script converted every test, without judging or rewriting any of them, and compared each expected
output with what the harness gives before writing the case.

- **The case.** A test's input as written, its config as front matter, and its expected output as
  the gold. The newline a triple quoted input started with is gone where the result is the same
  without it.
- **Formatted twice.** A test that formatted its own result again is one case with its first
  input: every case checks that its result is idempotent.
- **One case or several.** The tests of one file that formatted the same input with the same
  settings are one case. A test of several steps, or a parameterised one, is a case per step or
  argument.
- **The name.** The test's name, its issue number first, cut at the last dash before 60
  characters. Where that left two cases of a folder told apart by a `-2` alone, or by their folder
  alone, both took back the words that set them apart.
- **Negative.** A test whose expected output was its input is a case in `negative/`.
- **Only the end changed.** A test whose expected output was its input with only its end tidied, a
  final newline added or trailing spaces dropped, is a case in `negative/` too, with that expected
  output as its input. A gold would show nothing else, and what formatting does to the end of a
  file is what the cases under `settings/insert_final_newline/` are for. 148 cases are such.
- **Ignored.** An `[<Ignore>]` test that still does not pass is an ignored case, with its reason.
- **Defines.** A test that formatted one define combination merged its result with itself first,
  which moved its directives to column 0. A case keeps what each combination printed before the
  merge, so a per-define gold need not match that expected output there.
- **Not here.** What `Fantomas.Core.Tests` still holds is unit tests: of internals, of formatting a
  syntax tree without its source, of inputs large enough to overflow the stack, and of a parse
  error. A test that never ran, having no `[<Test>]`, was dropped, and so was an ignored test that
  formatted one define combination, which no gold can hold. Three of the tests that never ran give
  what they expected and are cases elsewhere now: `indent multiline lambda in parenthesis, 523` in
  `oak/Expr/ParenLambda/`, the signature file `should preserve quotes around type parameters, 2875`
  in `oak/Type/Var/negative/`, and `multiline field body expression where indent_size = 2, inherit
  record` in `settings/fsharp_multiline_bracket_style/cramped/Expr/InheritRecord/`.

The script, `scripts/convert.fsx`, and its record of what became of each test,
`src/Fantomas.Core.SnapshotTests/porting-ledger.tsv`, are in the commits of #3512. The record is
not complete:

- `BlankLinesAroundNestedMultilineExpressions.fs`, whose name does not end in `Tests`, was converted
  by a later version of the script that was not committed.
- The record was made before `main` gained three exception abbreviation tests in
  `TypeDeclarationTests.fs` (#3511), which were converted the same way when rebasing. Its line
  numbers for that file are from before them.
- `long array sequence` of `ListTests.fs` sat behind `#if RELEASE`, where the script did not look,
  and was converted by hand afterwards.
- The 148 cases whose result only changed at the end are recorded with the outcome `case`, under
  the path they had before they moved to `negative/`: look for the same name in the `negative/`
  folder beside it.
- The two tests that formatted twice, `should keep space before :` of `LetBindingTests.fs` and
  `should split constructor and function call correctly, double formatting` of
  `PatternMatchingTests.fs`, first became negative cases of their result, and were given their
  input back by hand.
- The eleven cases renamed to take back words the cut lost are recorded under their cut names.

## History

The tests as they last were:
[`src/Fantomas.Core.Tests` at 8bf81a1](https://github.com/fsprojects/fantomas/tree/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests).
Their history did not move with them. To see why a test was written, ask git about the file it was in:

```
git log --follow -- src/Fantomas.Core.Tests/CommentTests.fs
git blame 8bf81a1 -- src/Fantomas.Core.Tests/CommentTests.fs
```

The commit that added a test usually names the issue or the pull request behind it.

## What the old files said

A test file said more than its tests: why a group of tests exists, which style guide a layout follows,
what was still an open question. The conversion carried only the tests, so a folder whose file had
comments around its tests has a `README.md` holding them, each with the cases it was written above or
inside. Read them with this in mind:

- They are as they were written. Only their em dashes became other punctuation.
- "The test below" or "these tests" means the cases listed with the comment.
- They speak from the day they were written. "The current behavior results in a compile error" is
  the bug a test was written for, not what Fantomas does today: the case passes. A `TODO` is a
  question that was open then, and may be settled now.
- A link to a style guide or a document may have moved since.

Section labels that only repeat the names of the tests below them are kept too: they still say which
cases the file grouped together.
