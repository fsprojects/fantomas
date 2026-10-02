# Snapshot tests for Fantomas.Core: plan

## Resuming

State on 2026-10-02, branch `gold`, draft pull request https://github.com/fsprojects/fantomas/pull/3512.

The port is mechanical now. Porting by hand, folder by folder with an agent judging each test, was
dropped: nobody can review 3,000 judgements, so nothing could vouch that no test was lost.
`scripts/convert.fsx` converts the old tests instead, without judging any, and proves each one
against its case. The project README ("The ported tests") is the reference for how.

Where it stands:

- `cases/ported/<old test file>/` holds 2,873 cases (617 negative) made from 2,919 old tests.
  Every one of those tests' expected outputs was compared with the harness result before writing.
- `convert.fsx -- --check` regenerates everything in memory and fails on any difference with
  `ported/` or `porting-ledger.tsv`. It was shown to catch a changed gold and a stray file.
- No ignored test passes today. 6 became ignored cases (`name.ignore.fs`, gold = what the test
  expects, skipped with the reason, failing once it passes; README "Ignored cases").
- 242 tests are not converted, each with its reason in the ledger: 199 in unit test files,
  4 ignored ones that format one define combination, 9 `formatAST`, 5 without `[<Test>]`, 12 that
  call no helper, 6 that call one twice, 3 parameterised, 3 with a non-literal string, 1 that
  checks an exception.
- **One pull request.** The user wants the switch in this one PR, without a period where both
  suites live side by side. So no `--check` in CI: the converter runs a last time before the old
  tests go, and then goes itself.
- The 36 hand-written cases under `oak/` and `settings/` that no old test is behind stay, as
  extra cases. The 95 hand ports of old tests were deleted: `ported/` has those tests now.
- `scripts/ledger.fsx` and `scripts/review.fsx` are gone; the converter writes the ledger.

Decisions that stand:

- **Coverage per test.** AltCover's per-test tracking loses the test at the first async hop. What
  works: instrument Fantomas.Core, call each test in turn, and read and clear the recorder's static
  `Instance+I.visits` table in between. The whole old suite takes 18 seconds that way. Prototype in
  `/tmp/cov-probe/pertest.fsx` (not in the repo). Result: 630 of the old tests reach all 14,119
  points the suite reaches. Not used to drop tests: coverage says which code a test reaches, not
  which output it pins, and 642 of the rest are regression tests named after an issue.
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
3. **Coverage parity.** The snapshot cases plus the unit tests that stay must reach every point the
   old suite reaches, measured the same way. That is the gate for removing the old tests, and it
   replaces "100% of `SyntaxOak.fs`" as one.

## Phase 3: removing the old tests, in this pull request

Only when all of these hold:

- [ ] `convert.fsx -- --check` passes.
- [ ] Coverage parity holds.
- [ ] Every test the ledger lists as not converted is either kept as a unit test or dropped for a
      reason the user agreed to.

Then, in this same PR: delete the converted tests from `Fantomas.Core.Tests`, keep the unit tests
there, and delete `convert.fsx` and the ledger, which only mean something while the old tests exist.
Rewrite the README's "The ported tests" to say what `ported/` is without them. `ported/` stays as
it is; its folders can be split by node later, a rename at a time.
