---
name: upgrade-trial
description: Try a Fantomas upgrade against a real code base on this machine. Takes the path to a local repository, walks it from its pinned version up to latest stable, latest alpha, and a local build, formatting, checking and building at each step, committing on a test branch. Use to find out whether a change breaks or improves real projects before releasing it.
---

# Upgrade trial

Format a real code base with successively newer Fantomas versions and prove it still builds. This
is the check that catches what the test suite cannot: what our changes do to code nobody on this
team wrote.

**Input**: the path to a local git repository. Without one, ask for it rather than guessing.

## Never work on the branch they have checked out

This trial rewrites files across the whole repository and commits several times. Doing that on
someone's working branch is destructive, and the mess is theirs to clean up.

Before touching anything: record the current branch, confirm the working tree is clean, and create
a dedicated branch for the trial, `fantomas-upgrade-trial` or similar. If a branch by that name is
already there, reuse it only after checking it holds nothing but earlier trials. If the tree is
dirty, stop and say so; do not stash their work to get going. Leave the repository on the trial
branch when you finish and tell them which branch that is and which branch they were on, so
switching back is one command.

Never push. The commits are evidence for a decision, not something to publish.

## Before starting

Establish these, and report them back before running anything long:

- How to get back: the branch they started on.
- How it pins Fantomas: usually `dotnet tool restore` against `.config/dotnet-tools.json`. Note the
  version already pinned; that is rung zero.
- How it formats: usually `dotnet fantomas .`. Check `DEVGUIDE.md`, `CONTRIBUTING.md` or the CI
  yaml for the exact invocation, including which paths.
- How it builds: `./build.sh`, `build.cmd`, or a plain `dotnet build`.
- Whether it has a `.fantomasignore`. Large repos exclude a lot, and the exclusions tell you what
  they already know is awkward.

Say up front that the builds are long and CPU heavy, and run them in the background one at a time.

## Rung zero: the base state must pass its own check

Before touching any version, run the pinned version's `--check` and establish the scope it must
pass. When the docs and CI disagree on which paths to format, trust CI: that is the scope the
project actually enforces, and the rest is drift they have chosen to live with. FsAutoComplete's
CONTRIBUTING says to format `src/ test/`, but its CI has only ever checked `build.fsx src`; its
`test/` tree holds intentionally unparseable fixtures and has never been formatted. Reformatting
that scope on their behalf is churn they did not ask for, and can break fixture-sensitive tests in
ways a build will not catch. Only widen beyond the CI scope when they clearly want it.

If the check fails on the enforced scope, the base state is dirty, and the first commit on the
trial branch is fixing that: format with the pinned version, build, and commit as the rung-zero
baseline. Do this before any version bump, so each rung's diff isolates the version's effect
instead of inheriting drift. If the base state already passes, say so; that is information too.

Some files in scope may be genuinely unformattable: fixtures that are intentionally incomplete F#,
or code in syntax the pinned parser rejects (F# 7 relaxed indentation is a live example). Record
that set explicitly — file by file, with the reason — and compare each rung's check against it,
rather than demanding exit 0 from a repo that cannot give it.

## The rungs

For each of: **latest stable**, **latest alpha**, **a local build of the branch under test**:

1. Point the repository at that version. For published versions, edit the tool manifest and
   `dotnet tool restore`. For the local build, do **not** package and install it; build the CLI in
   Release and invoke the dll directly:
   `dotnet <fantomas>/artifacts/bin/Fantomas/release/fantomas.dll . --check`.
2. Confirm the version actually in use with `--version`. The commit hash in it is worth recording.
3. Format the repository.
4. Run the same command with `--check`. It must exit 0: the formatter has to agree with its own
   output. A failure here is an idempotency bug, and worth reporting rather than working around.
5. Build. Skip this when formatting changed nothing, since there is nothing for it to prove; say so
   rather than silently skipping.
6. Commit on the test branch: the manifest change together with whatever formatting changed. Never
   push.

Report per rung: files reformatted out of files considered, ignored, errored, whether `--check`
passed, and the build result.

## Traps this hit for real

- **Feeds.** Repos that restore from mirrors, dnceng for instance, lag behind nuget.org, so a
  freshly published version is not there yet. Add
  `--add-source https://api.nuget.org/v3/index.json` to `dotnet tool restore` rather than editing
  their `NuGet.config`, and note it in the commit message.
- **Package source mapping** makes `--add-source` fail outright. When installing to a
  `--tool-path`, run the install from a neutral directory so no repository `NuGet.config` applies.
- **Read our own CHANGELOG before blaming the branch.** A large diff is usually a documented
  breaking change in a setting default, not the work under test. Check whether the repository sets
  that setting for `[*.fs]` but not `[*.fsi]`, which is a common way to be surprised.
- **A version bump with zero changes is a result**, not a failure to find anything.

## Timing, if asked

Measure with `--check` so nothing is written, discard a warmup run, and interleave the versions to
cancel drift. Report the **minimum** of several runs, and prefer **wall clock**: user CPU time
varies by nearly 2x run to run on a laptop and will swamp any real difference. If the ranges of two
versions overlap, say the measurement was inconclusive instead of quoting a ratio. For a real
number, use `dotnet fsi build.fsx -- -p Benchmark`, which runs BenchmarkDotNet.
