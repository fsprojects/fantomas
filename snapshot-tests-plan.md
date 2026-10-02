# Snapshot tests for Fantomas.Core: plan

## Resuming

State on 2026-10-02, branch `gold`, draft pull request https://github.com/fsprojects/fantomas/pull/3512.

The switch is done in the working tree: the old formatting tests are converted and deleted.

- `cases/ported/<old test file>/` holds 2,950 cases (644 negative, 6 ignored) made by a converter
  that verified every old test's expected output against the harness. A last `--check` passed
  before the deletion, and coverage parity held (0 points lost). The converter and its ledger are
  deleted; their final versions (reading the test files from the fsproj, which caught
  `BlankLinesAroundNestedMultilineExpressions.fs`) are also kept in `/tmp/final-converter/`.
- `Fantomas.Core.Tests` keeps 216 tests: the 13 unit test files, plus the 10 syntax tree tests moved
  into `FormatAstTests.fs`, the 6 stack overflow tests into a new `StackOverflowTests.fs`, and the
  parse error test into `CodeFormatterTests.fs`. The 9 tests that never ran were dropped.
- `CoverageReach` (`scripts/reach.fsx`) now measures the unit tests and the cases, without parity.
  `Coverage` counts the snapshot tests for Fantomas.Core too; `CoverageOak` writes
  `syntaxoak-coverage.xml`.
- `scripts/format.fsx --define A,B` (or `no-defines`) prints one define combination before the merge,
  which the contributor docs on multiple defines now use in place of `formatSourceStringWithDefines`.
- Contributor docs, `AGENTS.md` and analyzer notes point at snapshot cases.
- Left for the end of the PR: delete this plan file (it was committed in `5f9e292b1`).

Decisions that stand:

- **Coverage per test.** `dotnet fsi build.fsx -- -p CoverageReach` (`scripts/reach.fsx`):
  AltCover's own per-test tracking loses the test at the first async hop, so the script calls every
  old test and every case itself against one instrumented Fantomas.Core, and reads and clears the
  recorder's `visits` and `samples` between them. Takes about 40 seconds after the build. Two traps
  it handles: the recorder runs in `Single` mode, so without clearing `samples` a point is only
  recorded for the first test that reaches it (the earlier prototype fell into this, and its "630
  tests reach everything" was wrong); and module initialisers run once per process, so a warm-up
  runs every class constructor first.
- **Parity holds** (2026-10-02): of the 10,810 points the old tests reach, the snapshot cases reach
  all but 459, and every one of those 459 is reached by an old test that stays (selection
  formatting, cursor, strict mode, unit tests). No converted test reached anything its case does not.
- **Bugs.** A bug found while working on this is not reported upstream and not fixed: it is a bug
  once a user runs into it.
- **No syntax tree comparison.** Fantomas changes the tree on purpose sometimes.
- **Trivia.** Which node trivia attaches to is not checked. Comments, conditional directives and
  warn directives must not be lost.
- **Tooling** goes in `scripts/`, not in the test project, which stays plain tests.

## Next

1. **Coverage for new work.** A script on top of `reach.tsv`: for the cases named, or the uncommitted
   ones, list every changed line of `src/Fantomas.Core` that no case reaches and every changed branch
   where only one side is taken. Then the README and `AGENTS.md` say: when you fix a bug or change
   printing, add a case under `oak/` or `settings/`, run it, and close what it reports.
2. **Delete this file** in the last commit of the PR.
