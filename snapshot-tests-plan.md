# Snapshot tests for Fantomas.Core: plan

## Resuming

State on 2026-10-02, branch `gold`, draft pull request https://github.com/fsprojects/fantomas/pull/3512.

The port is mechanical now. Porting by hand, folder by folder with an agent judging each test, was
dropped: nobody can review 3,000 judgements, so nothing could vouch that no test was lost.
`scripts/convert.fsx` converts the old tests instead, without judging any, and proves each one
against its case. The project README ("The ported tests") is the reference for how.

Where it stands:

- `cases/ported/<old test file>/` holds 2,940 cases (640 negative, 6 ignored) made from 2,942 old
  tests.
  Every one of those tests' expected outputs was compared with the harness result before writing.
- `convert.fsx -- --check` regenerates everything in memory and fails on any difference with
  `ported/` or `porting-ledger.tsv`. It was shown to catch a changed gold and a stray file.
- No ignored test passes today. 6 became ignored cases (`name.ignore.fs`, gold = what the test
  expects, skipped with the reason, failing once it passes; README "Ignored cases").
- 225 tests are not converted, each with its reason in the ledger: 199 in unit test files, and
  26 more. 9 never run and go (5 without `[<Test>]`, 4 ignored define tests: the user agreed). 17
  stay as unit tests: 9 `formatAST` and 1 hand-built tree (`FormatASTAsync`, no source), 6 stress
  tests (1,000 declarations, a very long string; they only check for no stack overflow), 1 that
  checks a parse error throws.
- **One pull request.** The user wants the switch in this one PR, without a period where both
  suites live side by side. So no `--check` in CI: the converter runs a last time before the old
  tests go, and then goes itself.
- The 36 hand-written cases under `oak/` and `settings/` that no old test is behind stay, as
  extra cases. The 95 hand ports of old tests were deleted: `ported/` has those tests now.
- `scripts/ledger.fsx` and `scripts/review.fsx` are gone; the converter writes the ledger.

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

1. **The 49 tests that are not unit tests and not converted.** Small converter extensions would
   take some: a test calling a helper twice is two cases, a parameterised test one case per
   `TestCase`. The rest stay listed. Decide per reason, not per test.
2. **Coverage for new work.** Turn the per-test measurement into a script: for the cases named, or
   the uncommitted ones, list every changed line of `src/Fantomas.Core` that no case reaches and
   every changed branch where only one side is taken. Then the README and `AGENTS.md` say: when you
   fix a bug or change printing, add a case under `oak/` or `settings/`, run it, and close what it
   reports.
3. **The leftovers are decided.** The converter learned file helpers, several steps per test,
   parameterised tests, interpolated strings and formatting a result again, which took 17 more.
   The 9 that never run go with the deletion; the 17 above and the unit tests stay.

## Phase 3: removing the old tests, in this pull request

Only when all of these hold:

- [ ] `convert.fsx -- --check` passes.
- [x] Coverage parity holds (`CoverageReach`, `artifacts/coverage/parity.md`); rerun before deleting.
- [ ] Every test the ledger lists as not converted is either kept as a unit test or dropped for a
      reason the user agreed to.

Then, in this same PR: delete the converted tests from `Fantomas.Core.Tests`, keep the unit tests
there, and delete `convert.fsx` and the ledger, which only mean something while the old tests exist.
Rewrite the README's "The ported tests" to say what `ported/` is without them. `ported/` stays as
it is; its folders can be split by node later, a rename at a time.
