---
description: Bump the vendored FCS commit hash one upstream SyntaxTree commit at a time
argument-hint: "[check|fast]"
allowed-tools: Bash(gh api:*), Bash(git:*), Bash(dotnet:*), Bash(grep:*), Bash(sed:*), Bash(mkdir:*), Bash(sort:*), Bash(comm:*), Read, Edit
---

Update the vendored F# compiler sources in `src/Fantomas.FCS` by moving `FCSCommitHash`
forward to the **next** upstream commit that touched `src/Compiler/SyntaxTree`.

Advance **one meaningful commit per invocation**. Never jump straight to `main`: a single hash
bump that crosses many upstream changes makes a broken build impossible to attribute.

A commit is meaningful only when it touches a file Fantomas actually vendors. A SyntaxTree commit
that changes nothing in the vendored file list cannot change the build, so **skip it** and keep
walking to the next one. Skipping is safe precisely because the landing hash still contains the
skipped commits, their content just does not reach `src/Fantomas.FCS`. Never skip a commit you
have not proven to be a no-op this way.

This command is meant to be run **repeatedly**, once per meaningful upstream commit, until the
vendored copy has caught up. Most steps will need no repo change at all, the hash moves and
everything still builds. A few will need real work. Treat a clean step as the normal case and
say so plainly, do not go looking for something to fix.

Every run must open and close with the same progress line, so the user can see the catch-up
shrinking across invocations:

```
FCS catch-up: <n> SyntaxTree commit(s) behind dotnet/fsharp main
```

`$ARGUMENTS`:

- `check` — report the pending commits, marked as vendored or no-op, and stop, change nothing.
- `fast` — skip the full default build pipeline in step 5, run only the vendored project build
  and the Fantomas.Core unit tests. Use this when walking many commits in a row. The full
  pipeline is still the default, and is worth running at least on the last step of a walk.

## 0. The `.deps` cache

`.deps/` is gitignored, so keep the walk's scratch data in `.deps/.fcs-walk/`. Create it if
missing. Two things live there:

`vendored-files.txt` — the list of files Fantomas actually vendors, taken from the `Init` pipeline
file list in `build.fsx`, which is the single source of truth for what gets downloaded:

```
mkdir -p .deps/.fcs-walk
grep -oE '"src/Compiler/[^"]+"' build.fsx | tr -d '"' | sort -u > .deps/.fcs-walk/vendored-files.txt
```

Regenerate it whenever `build.fsx` is newer than the cache file, otherwise reuse it. It is about
85 paths.

`commits/<sha>.tsv` — the changed-file list of one upstream commit. A commit's file list never
changes, so this is cacheable forever and saves an API round trip on every later run. Read from
the cache when the file exists and is non-empty, otherwise fetch and write it.

## 1. Read the current hash

```
grep -n FCSCommitHash Directory.Build.props
```

Get its commit date upstream (needed to list commits after it):

```
gh api repos/dotnet/fsharp/commits/<current-hash> --jq '.commit.committer.date, (.commit.message | split("\n")[0])'
```

## 2. Find the pending SyntaxTree commits

```
gh api "repos/dotnet/fsharp/commits?path=src/Compiler/SyntaxTree&sha=main&since=<date>&per_page=100" \
  --paginate --jq '.[] | [.sha, .commit.committer.date, (.commit.message | split("\n")[0])] | @tsv' | tail -r
```

`tail -r` reverses to oldest-first (this is macOS, there is no `tac`). `since` is inclusive, so
the first row is the current hash itself. Drop it.

If nothing is left, report that Fantomas is up to date with the SyntaxTree folder and stop.

## 2b. Pick the target, skipping no-op commits

Walk the pending list oldest-first. For each candidate, get its changed-file list, from
`.deps/.fcs-walk/commits/<sha>.tsv` when cached, otherwise:

```
gh api repos/dotnet/fsharp/commits/<sha> --jq '.files[] | [.status, .filename, .additions, .deletions] | @tsv' \
  > .deps/.fcs-walk/commits/<sha>.tsv
```

Intersect the filenames with `.deps/.fcs-walk/vendored-files.txt`. Empty intersection means the
commit cannot affect the build, so it is a **no-op**: skip it and move to the next candidate.
The first candidate with a non-empty intersection is this run's target.

A no-op is common. Many SyntaxTree commits only touch files Fantomas does not vendor, or only
touch tests and release notes. Skipping them is the point of this step, it saves a full build
cycle that could not have told you anything.

If every pending commit is a no-op, bump straight to the newest one, say that the whole remaining
range was vendor-neutral, and still build to prove it.

Open with the progress line and the numbered list, oldest first, marking skipped commits and the
one this run will take:

```
FCS catch-up: 17 SyntaxTree commits behind dotnet/fsharp main

  --   1. 9487d36e  2026-05-20  Fix #17904 and #19020 (#19738)                       no vendored file
  ->   2. f15535b5  2026-06-03  Fix XmlDoc validation for get/set property pairs (#19884)
       3. bc8a51b7  2026-06-04  Fix parser error for anonymous record type aliases ... (#19762)
      ...
```

Name the skipped commits explicitly, do not silently drop them. The user is walking this range to
understand it, a skip is information.

The count is recomputed from upstream on every run, so it shrinks by one per successful step, by
more when commits were skipped, and it can also grow when new commits land on `main`. Say which
it did if the number moved unexpectedly.

If `$ARGUMENTS` is `check`, classify every pending commit this way, print the list with the no-ops
marked, and stop. Answer the question "how much of this backlog is real work", the cache makes
the second such run cheap.

## 3. Summarise what the target commit changes

Before touching anything, give the user a real overview, not just a subject line:

You already have the file list from step 2b, in `.deps/.fcs-walk/commits/<target-sha>.tsv`.

Report:

- The commit subject, the PR number and its link (`https://github.com/dotnet/fsharp/pull/<n>`).
- Which files under `src/Compiler/SyntaxTree` changed, and how much.
- Whether any file was **added, removed or renamed** anywhere under `src/Compiler`. Those need a
  matching edit in the file list in `build.fsx` (the `Init` pipeline) and in the `Compile`
  items of `src/Fantomas.FCS/Fantomas.FCS.fsproj`. Both lists are explicit, nothing is globbed.
- The parts of the diff that matter to Fantomas: changes to `SyntaxTree.fs(i)`,
  `SyntaxTrivia.fs(i)`, `SyntaxTreeOps.fs(i)`, `pars.fsy` and `ParseHelpers.fs`. A new or changed
  trivia field, a new `Syn*` case, or a changed case shape ripples into `ASTTransformer.fs`,
  `SyntaxOak.fs` and `CodePrinter.fs`.
- Whether files outside `SyntaxTree` changed too. The hash controls **every** vendored file, not
  only the SyntaxTree folder, so a SyntaxTree-scoped walk can still pull in unrelated compiler
  changes and break the build.

Fetch the actual patch for the interesting files when the file list alone is not enough to
explain the change:

```
gh api repos/dotnet/fsharp/commits/<target-sha> --jq '.files[] | select(.filename == "src/Compiler/SyntaxTree/SyntaxTrivia.fsi") | .patch'
```

## 4. Bump the hash

Replace the value of `<FCSCommitHash>` in `Directory.Build.props` with the full target sha.

## 5. Download the new sources and build

```
dotnet fsi build.fsx -- -p Init
```

This downloads the compiler files at the new hash into `.deps/<hash>` (gitignored) and rewrites
`FSharp.Compiler` to `Fantomas.FCS` in them. It must run before any build, otherwise the
`.deps/<hash>` folder does not exist.

Then clear the vendored project's build state, so nothing generated from the previous hash
survives into this one:

```
dotnet clean src/Fantomas.FCS/Fantomas.FCS.fsproj
```

`Init` skips any file that already exists, so revisiting a hash leaves the sources with their
original download time. `FSComp.txt` generates the `SR` module, and MSBuild regenerates it only
when the input looks newer than the generated output. Revisit a hash whose sources predate the
last build and the stale `SR` is reused, giving errors like

```
error FS0039: The type 'SR' does not define the field, constructor or member 'featureXyz'
```

which look like a real breakage in the target commit and are not. Cleaning removes the whole
class of confusion. This bites whenever the walk moves backwards, which it does when bisecting a
regression. The full pipeline is immune because its `Clean` stage deletes `artifacts` outright,
so this only matters for the standalone compile check below.

Then a fast compile check of the vendored project only, which fails in seconds rather than
minutes when a signature changed:

```
dotnet build src/Fantomas.FCS/Fantomas.FCS.fsproj
```

Then the default build pipeline:

```
dotnet fsi build.fsx
```

This runs check-format, release build, unit tests, pack and docs. It is slow. Tell the user it
is running before you start it.

When `$ARGUMENTS` is `fast`, run this instead of the default pipeline, and say in the report that
the full pipeline was skipped:

```
dotnet test src/Fantomas.Core.Tests/Fantomas.Core.Tests.fsproj
```

## 6. On failure, hand over

If any step fails, **stop and hand the problem to the user**. Do not guess at compiler-semantics
fixes, and do not revert the hash.

A green pipeline is not proof the step is good. When the target commit changes how the parser
*shapes* the tree, rather than only what it computes, probe the affected syntax by hand with
`scripts/format.fsx` before declaring success. The test suite only covers syntax someone already
wrote a test for, and the failure mode of a shape change is silently dropped source, which no
existing assertion notices. Round-tripping the construct through the local build takes seconds:

```
dotnet build src/Fantomas/Fantomas.fsproj -v quiet
dotnet fsi scripts/format.fsx <file>
```

Treat dropped or duplicated source found this way exactly like a build failure: stop and hand over.

### Parse the new freedom, do not print it

Some upstream commits make the parser accept a layout it used to reject. Verify that the newly
accepted syntax round-trips unchanged and stop there. Do not change a layout decision or add a
`CodePrinter` case to take advantage of the new freedom, however much nicer the output would look.

The vendored compiler is always ahead of the compiler in the published .NET SDK that users run.
Output only the newer compiler accepts does not compile for them. Adopting a new construct is a
deliberate separate change for much later, with its own settings discussion, not a side effect of
a hash bump.

### Pin every behaviour change with a test

A behaviour change that no test covers is the dangerous kind, the suite stays green while the
formatter misbehaves. Before handing over, **add the missing test**, in the existing test file
that already owns that syntax (attributes go in `AttributeTests.fs`, and so on). Match the
surrounding idiom, `formatSourceString ... config |> should equal`, and name it after the syntax,
not after the upstream commit.

Assert the **correct** output, the one that round-trips the user's source. That test fails right
now, and that is the point: it pins the regression so it cannot be forgotten, and it turns green
the moment someone fixes it. Do not weaken the expectation to match the current broken output,
and do not `Ignore` the test.

Do this for the syntax that is actually broken, and also for any near neighbour the probe showed
still works but no test covered. The one that works costs nothing and stops the next walk from
depending on luck the way this one did.

Record it so later steps can tell it apart from new breakage, appending one line to
`.deps/.fcs-walk/open-regressions.tsv` as `<sha>`, `<test name>`, `<test file>`, `<one-line
description>`.

### A known failing test is not a failed step

Once a regression is pinned, the `UnitTests` stage fails on every later step until it is fixed,
and `Pack` never runs. That must not stall the walk.

When the pipeline fails, read the failing test names and compare them with
`open-regressions.tsv`. If every failure is already recorded there, the step is fine, say so
plainly, name the pinned regression and carry on. If even one failure is not in that file, it is
new breakage from this step, so stop and hand over as above.

Present: the failing step, the actual error output (trimmed to the relevant compiler errors),
and your reading of which upstream change caused it.

You may apply the purely mechanical fixes, but say so and show the diff:

- A file added or removed upstream: add or remove the matching entry in the `Init` file list in
  `build.fsx` and the matching `Compile`/`Link` pair in `src/Fantomas.FCS/Fantomas.FCS.fsproj`.
  Order matters in both, F# compilation order follows the upstream order.
- A pure rename with no shape change.

Anything else is the user's call. Common non-mechanical breakage:

- A trivia record gained or lost a field, so `ASTTransformer.fs` no longer compiles.
- A `Syn*` union case changed shape, so pattern matches in `ASTTransformer.fs` fail `FS0025`
  (incomplete matches are errors in this repo).
- `src/Fantomas.FCS/Parse.fs` drifted from the upstream parser entry points it mirrors.
- A test asserts on formatting that the parser now produces differently. That may be a genuine
  improvement, discuss it before rewriting the expectation.

## 7. On success

Say which of the two outcomes this was, in one line, before any detail:

- **Clean bump** — only `Directory.Build.props` changed, everything built. This is the common
  case and needs no discussion. `git status --short` proves it.
- **Bump with fixes** — list what else had to change and why.
- **Bump with a pinned regression** — it built, but a probe found a behaviour change and there is
  now a failing test naming it. Say which test and what it asserts.

Then report:

- The old and new hash, and the upstream subject and PR link.
- Which commits were skipped as no-ops on the way, if any.
- Anything the user should keep in mind for the steps ahead, for example an upstream change that
  compiles today but will need Oak or `CodePrinter` work once a later commit builds on it.
- Any **open regression carried over from an earlier step**, in one line, until it is resolved. A
  green pipeline does not mean the walk is healthy, and a problem found three steps ago is easy
  to forget once the tree keeps building.
- The closing progress line and the next target:

```
FCS catch-up: 16 SyntaxTree commits behind dotnet/fsharp main
Next: f15535b5  Fix XmlDoc validation for get/set property pairs (#19884)
```

Leave the work uncommitted and **do not ask whether to commit a clean bump**. The user walks
several steps in a row and batches the committing themselves, asking every time is noise. Only
raise committing when a step needed real fixes or pinned a regression.

A pinned test belongs to the step whose upstream commit **caused** the behaviour change, not to
whatever step the walk had reached when the test was written. Those differ whenever a probe runs
late, or a regression is noticed a step or two after it landed. `open-regressions.tsv` records the
causing sha for exactly this reason, use it. If the walk has already moved past that step and the
bumps are still one uncommitted blob, say so and offer to reconstruct the steps as separate
commits, rather than letting the history blame an innocent later commit.

When the user does ask for a commit, follow the existing convention in this repo:

```
Update FCS to '<upstream commit subject>', commit <full-hash>
```

One commit per step. Do not fold several hash bumps into one commit, the point of walking is that
a later bisect lands on a single upstream change.

Finally, ask whether to run this command again for the next pending commit. If the user answers
with something like "again", "next" or "continue", that is this command, not a new conversation.
