---
name: pr-description
description: Write or update a pull request's description, showing what changes for users with real before and after output.
disable-model-invocation: true
---

# Pull request description

**Input**: a pull request URL. The result is a draft in `tmp/pr-description/<n>.md`, shown to the
author, and posted as the whole description only when they say yes. The **author** is the person
running this session, usually the one who wrote the pull request: every question goes to them here,
and only their yes posts.

A description tells a reader outside the work two things the diff cannot: what changes for people
running Fantomas on their code (the **impact**), and why it was done this way (the **intent**). The
impact is measured: every before and after in it is output a command printed. The intent comes from
people: a diff shows what changed, never why.

Measure widely, show little. Every claim gets a run, but the description shows each kind of change
once, with the smallest input that shows it.

Everything runs from the repository root.

## 1. Gather

```bash
gh pr view <url> --json number,title,body,baseRefOid,headRefOid,commits,closingIssuesReferences,files
```

- **Head** is `headRefOid`; **base** is `git merge-base <baseRefOid> <headRefOid>`. When the checkout
  lacks either commit, give the user the `git fetch` command that gets it and stop.
- Read the body, every commit message, and every linked issue. Issues filed with the fantomas-report
  skill already say which versions fail and which variations do not.
- If this session is where the work happened, collect what was decided in it: the options weighed,
  the answers to `question.fs`, what was left out on purpose.

When `tmp/pr-description/<n>/` is left from an earlier run, start over:

```bash
rm -rf tmp/pr-description/<n>.previous && mv tmp/pr-description/<n> tmp/pr-description/<n>.previous
```

The scripts `compare.fsx` wrote name that run's base and head, so the rerun in step 6 would check
them against those and pass: measure again. In step 3, copy back from `<n>.previous/` the `inputs/`
and `compile/` you still need, with the `commands/` scripts by-hand.md recorded for them.

Done when you have a list of **claims** (each thing the description will say about output, from any of
these sources) and a list of **open whys** (each choice this pull request made whose reason none of
them gives).

## 2. Build both sides

```bash
dotnet fsi ${CLAUDE_SKILL_DIR}/build.fsx <base>
dotnet fsi ${CLAUDE_SKILL_DIR}/build.fsx <head>
```

This builds `src/Fantomas.Core.SnapshotTests`, which the cases need. When the pull request changes the
command line tool or the daemon, also run `build.fsx <sha> src/Fantomas/Fantomas.fsproj` for each sha,
after its first build: two builds of one sha at once unpack into the same folder. A sha for which
`git cat-file -e <sha>:src/Fantomas.Core.SnapshotTests` fails has no cases and gets the tool build
alone. A first build takes minutes, so run it in the background.

Done when every build prints its tree path.

## 3. Measure

```bash
dotnet fsi ${CLAUDE_SKILL_DIR}/compare.fsx <n> <name> <base> <head> format <input> [--editorconfig '<settings>']
dotnet fsi ${CLAUDE_SKILL_DIR}/compare.fsx <n> <name> <base> <head> cli <sample-folder> <arguments...>
```

Give each measurement a `<name>` of its own: a name used twice keeps only the last run.

`format` mode takes settings only from a case's front matter or from `--editorconfig`. Every input
you write sits in a folder of its own, `tmp/pr-description/<n>/inputs/<name>/`, beside an
`.editorconfig` that says `root = true` (the repository's own would apply otherwise) and holds,
under `[*.{fs,fsi}]`, the settings it needs: that file is for the tool and for released versions,
and `format` mode gets the same settings through `--editorconfig`.

When a side does not run, `compare.fsx` prints its message, and the message decides:

- A front matter setting, or value, the base does not know (a pull request adding one): pass the
  same case with its front matter lines, as written, as `--editorconfig`. Both sides then read them
  the way the tool does, and the base passes over what it does not know.
- A format script that is missing or does not compile: measure in `cli` mode. Build
  `src/Fantomas/Fantomas.fsproj` for both shas if step 2 did not, copy the case without its front
  matter into `inputs/<name>/` beside its `.editorconfig` (the tool reads front matter as a trivia
  block comment), and pass that folder with the file name as the argument.

What to run:

- The cases the pull request adds or whose golds it changes, one per line:
  ```bash
  git diff --name-only <base> <head> -- 'src/Fantomas.Core.SnapshotTests/cases/*.fs' \
    'src/Fantomas.Core.SnapshotTests/cases/*.fsi' \
    | sed -E 's/(\.[^./]+)?\.gold\.(fsi?)$/.\2/' | sort -u \
    | while read -r f; do git cat-file -e "<head>:$f" 2>/dev/null && echo "$f"; done \
    | grep -Ev '\.ignore\.fsi?$' || true
  ```
  A gold, added, changed or deleted, stands for its case. The last two lines drop what is not a case
  at head: a deleted case, the gold of an ignored case, and the ignored case itself, a bug the pull
  request does not fix. Empty output means the pull request changes no case.
  Pass each case as it is, from the head tree (`tmp/pr-description/<head>/src/...`): its front
  matter gives the settings. These are the examples the description shows first.
- Each "stays the same", "was already kept" and "left alone" claim, as a small input in
  `inputs/<name>/`. A claim copied from an issue can be off: run it.
- A command line change: a sample folder, `inputs/<name>/`, with the files the run needs and an
  empty `.fantomasignore` beside the `.editorconfig`, so the repository's ignore file stays out too.
  `cli` mode also diffs the files each side left.
- A claim about released versions, or about what the compiler accepts: see
  [by-hand.md](by-hand.md).

When this pull request chose between layouts, write each option as complete code. Base and head
outputs are real; an option nobody produced is written by hand, compiled as by-hand.md describes, and
measured with `compare.fsx` like any input, passing the compile check's `.fsx` itself under a name of
its own (`option-c-format` beside the compile check's `option-c`): its head side
says what Fantomas formats it to, and whether that is stable. Quote the option without the stubs the
compile check needed.

Done when every claim has a run whose output agrees with it, or the claim is dropped or corrected.
A run that contradicts what the author wrote (in the body, a changelog entry, a doc comment) is a
question for them in step 5, not a silent correction.

## 4. FCS

When `git diff <base> <head> -- Directory.Build.props` changes `FCSCommitHash`:

```bash
gh api --paginate repos/dotnet/fsharp/compare/<old>...<new> --jq '.commits[].sha'
gh api repos/dotnet/fsharp/commits/<sha> --jq '[.files[].filename]'
gh api repos/dotnet/fsharp/commits/<sha>/pulls --jq '.[] | "\(.number) \(.title)"'
```

Look at the commits that touch a file the repository's `Init` pipeline downloads, its list under
"Download FCS files" in `git show <head>:build.fsx`: those are the compiler sources Fantomas vendors.
Done when each dotnet/fsharp pull request that changes what the parser gives Fantomas is named with
its link and a line on what it adds, and the rest is one sentence with a count (such as fourteen
removals of language flags that were always on).

## 5. Find the why

Only choices this pull request made need one. A layout Fantomas already gave is not a choice here.
For each open why, in order:

1. The session, if the work happened in it.
2. A rule that fits: `docs/docs/end-users/DesignDecisions.md`, `RuleOfMultitudes.md`, or the style
   guides through the `style-guides` skill. Quote it and link it.
3. Invalid code: when the other options do not compile, their compiler errors are the reason.
4. The commit messages and the explanatory comments beside the change, for a choice that is not
   about layout.
5. Ask the author, one question at a time, each with its options as complete code.

When the pull request chose a layout, check it against the style guides as well. A layout a guide
marks ❌ needs that said, with the reason it was taken anyway.

Done when every open why has an answer from one of these.

## 6. Write

Write `tmp/pr-description/<n>.md` in the shape below: the description and nothing else, since it
becomes the whole body. Then rerun every command and compare, in a subshell so the working
directory stays the repository root:

```bash
(cd tmp/pr-description/<n> && rm -rf out.previous && mv out out.previous && mkdir out \
  && { for f in commands/*.sh; do PR_DESCRIPTION_RERUN=1 sh "$f" > /dev/null; done; \
  diff -r out.previous out; })
```

With no `commands/`, as for a pull request without user impact, there is nothing to rerun: say so
and skip it.

Output that differs between runs (warnings from files formatted in parallel, timestamps) is a
finding: say so in the description rather than show one run as the output.

Show the draft, and change it until the author says yes.

Done when `diff -r` prints nothing or the draft names each difference, and the author has said yes.

## 7. Post

Only after the author's yes to this draft:

```bash
gh pr edit <url> --body-file tmp/pr-description/<n>.md
```

The draft replaces the whole body. Done when `gh pr view <url> --json body --jq .body` prints the
draft.

## 8. Clean up

When the run ends, posted or not, delete everything it left: the work folder, the draft, both
builds and their logs. `<base>` and `<head>` are the full shas the build folders are named after.

```bash
rm -rf tmp/pr-description/<n> tmp/pr-description/<n>.previous tmp/pr-description/<n>.md \
  tmp/pr-description/<base> tmp/pr-description/<base>.log \
  tmp/pr-description/<head> tmp/pr-description/<head>.log
rmdir tmp/pr-description 2>/dev/null || true
```

Done when none of those paths exists.

## The shape

Write for someone running Fantomas, not someone reading the code. Each section appears only when it
has something to say. Most descriptions fit in 80 lines; one that weighs options can take twice that.

**Opening paragraph**, no heading. Two or three sentences on what kind of problem this is and what
kind of fix, in user terms; the examples show the details, so leave the layout out of the words. For
the command line tool or the daemon, say so in the first sentence. For a regression, the versions
that work and the ones that do not. Then a `Fixes #n` line for each issue it closes.

**`## FCS update`**, first, when the compiler sources move: the old and new sha, each dotnet/fsharp
pull request that changes the parse with a link and a line on what it adds, then the rest in one
sentence. What the update made possible follows as its own section.

**`## What changes for you`**, the impact, when users see a difference (`## What changes on the
command line` and `## In an editor` for the tool and the daemon). It opens with one line saying which
sha the `-` lines of its diffs are and which the `+`. Then one `###` per kind of change:

- the input, in a block of its own language (`fsharp`, `ini` for an `.editorconfig`): a case's
  front matter settings go in the `ini` block under `[*.{fs,fsi}]`, and its `#` description and `;`
  comments are left out; a case without settings gets no `ini` block;
- the `diff` block from `compare.fsx`, where `cli` mode prints the sample folder as `sample`: say so
  where that path appears;
- under it, what the before did beyond the layout: what validation rejected and that Fantomas left
  the file unchanged, or the second pass when it was not stable.

After the changes, what stays the same and what is left alone on purpose: a sentence each, and an
example only where the reader could not guess the output. A variation that formats like an example
already shown is a sentence, not another example. What cannot be run, such as an editor receiving a
daemon notification, is described from the code, saying it was not run.

**`## Why this way`**, the intent, when this pull request chose: the rule that chose, quoted and
linked, then each option as complete code saying whether it compiles and is stable, and which ones
are real output and which are written by hand. A choice that is not about layout (a threshold, a
message, a protocol shape) gets its reason in a sentence or two.

Leave out how the code works, unless the idea is notable in itself and not routine plumbing: the
code is there for the curious. Leave out test counts, case names and the commands that measured
it.

No local path, absolute or relative, appears anywhere in it: not the work folder, not a run folder,
not a scratch file's name. A sample folder is `sample`, and a compiler error is quoted from its
`error FS` on.
