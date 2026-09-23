---
name: fantomas-report
description: Turn F# code that Fantomas fails to format into a minimal sample and a prefilled GitHub issue for fsprojects/fantomas, in the format the fantomas-tools "Create an issue" button produces. Use when Fantomas reports that its output did not pass validation, throws, is not idempotent, or loses code, and the problem should be reported upstream.
---

# Report a Fantomas bug

**Input**: a file, or a snippet, that Fantomas fails on. The result is a GitHub issue form opened in
the browser, filled in the way https://fsprojects.github.io/fantomas-tools/#/fantomas/main fills it.
The user reads it and presses submit themselves: never file the issue with `gh` or any other way.

Not every failure is worth an issue. A good part of this skill is telling the user that, and what to
change in their code instead.

## Setup

Everything below runs from this skill's folder, the one this file is in. Call it `<skill>`.

**.NET.** Trying a sample against released versions uses `dotnet tool exec`, which needs the .NET 10
SDK or later. Check with `dotnet --version`; without it, say what is missing and stop.

**Script runtime.** `report.ts` needs one package, pinned in `package.json`.

- Use Bun when `bun --version` answers: `bun install` in `<skill>` once, then `bun report.ts ...`.
- Otherwise use Node 22.18 or later, which runs TypeScript as is: `npm install` in `<skill>` once,
  then `node report.ts ...`.
- With neither, recommend installing Bun (https://bun.sh) and stop.

**A Fantomas clone.** Shrinking a sample means formatting it dozens of times, and a local build does
that in a second where a round trip to the online tool takes several. It also has the diagnostic
scripts (`scripts/ast.fsx`, `scripts/oak.fsx`, `scripts/format.fsx`) and is where a fix would go.

- If the current repository is a Fantomas clone (`git remote -v` shows `fsprojects/fantomas`), use it.
  Otherwise ask the user where their clone is.
- Without one, ask permission to clone `https://github.com/fsprojects/fantomas`, and where to. Do not
  clone without a yes.
- Bring it up to date with `git pull` on `main` only if its working tree is clean, and ask first when it
  is not on `main`: it may be someone's work in progress.
- Build it: `dotnet build src/Fantomas/Fantomas.fsproj`. The CLI lands in
  `artifacts/bin/Fantomas/debug/fantomas.dll`, run as `dotnet <that path> <file>`.
- If the user declines, carry on without it: shrink against released versions with
  `dotnet tool exec -y fantomas@<version> -- <file>` and against main with `report.ts check`.

## 1. Shrink the sample

A report is only as useful as its sample is small, and GitHub rejects issue links past a few KB, so
this step is the work. Always format a copy: Fantomas rewrites the file in place.

First run `dotnet fantomas doctor <file>` (Fantomas 8 and later) on the original, from the user's
project. It lists the settings their `.editorconfig` files set. A copy formatted elsewhere loses
those, so check the failure still happens with the defaults. If it only happens with one of those
settings, keep that setting for the rest of the work and pass it to `report.ts` as `--setting`, under
its Fantomas name (`max_line_length` is `MaxLineLength`). An unknown name makes `report.ts` print the
valid ones.

- Start from the construct the error points at, not the whole file. The failing output line maps back
  to a declaration; take that declaration alone.
- Drop lines while it keeps failing. A short script that deletes one line at a time and keeps the
  deletion if the failure stays is faster than doing it by hand. Check that it fails for the same
  reason: a cut that leaves code which does not parse "fails" too.
- Then shorten names, strip comment text to `// c`, and drop the enclosing `let` or module when a
  top-level expression still fails.
- Try the obvious variations and note which ones matter: blank line instead of comment, no
  parentheses, one operand fewer, `if` instead of `match`. These go in the description.
- Watch the output, not only the verdict. Output that parses can still have lost a comment or changed
  the code. That is a bug too, and often the worse one.

## 2. Is it still a bug, and is it new?

- Run it against released versions: `dotnet tool exec -y fantomas@<version> -- <copy>`, for the
  latest stable and the last release of the previous major. Failing on the latest but not on the
  older one makes it a regression; say so in the report.
- Passing on the latest stable while the user's project pins an older one means it is already fixed.
  Tell them which release fixed it and how to update (`dotnet tool update fantomas`; a `NuGet.config`
  that clears its sources needs `--add-source https://api.nuget.org/v3/index.json`), instead of
  reporting.

## 3. Not a Fantomas bug to report

Some code is written in a way no formatter can keep. Recognize these, tell the user, and propose a
refactor of their code. The user decides; report only if they still want to.

**Nesting that depends on a define.** Fantomas formats each `#if` combination on its own and merges the
results line by line, so code after `#endif` gets one indentation for all of them. When a construct
that opens a body exists in only one branch, the code after `#endif` is nested in one variant and not
in the other, and no indentation fits both. The signature is a body-opening line right before
`#endif`:

```fsharp
#if A
if a then
#endif
    b
```

```fsharp
#if A
match x with
| _ ->
match y with
| _ ->
#endif
x
```

Refactors that work: move the define into a value (`let enabled = #if A ... #else ... #endif` and then
an unconditional `if enabled then`), or merge the nested matches into one tuple match so that only one
clause is left open before `#endif`. Keep the code's evaluation order: inside a `seq` or `async`, bind
the value inside it, not before it. Point the user to
https://fsprojects.github.io/fantomas/docs/end-users/ConditionalCompilationDirectives.html, which
describes this limitation and the other ones directives have.

**Syntax only FSharp.Core may use.** Inline IL with a type argument (`(# "..." type ('T) x : 'T #)`),
`cons.( :: ).1 <- t` and similar parse only when compiling FSharp.Core. Fantomas does not support them
on purpose; such files belong in `.fantomasignore`.

## 4. Check it against main

```bash
bun report.ts check <sample> [--fsi] [--setting MaxLineLength=60]
```

This formats with the deployed main branch, which is what a maintainer gets when they click the link
in the issue. The outcome is one of:

- `invalid-output`: the result does not parse. Report.
- `error`: Fantomas threw. Report.
- `not-idempotent`: formatting the result again changes it. Report; the issue then shows the code, the
  first format and the second.
- `formatted`: nothing wrong mechanically. Report if the output lost or changed code, or if the layout
  is wrong and you can say why.
- `invalid-source`: Fantomas' parser rejects the sample. Usually the shrinking broke it; go back. When
  the code compiles with the F# compiler and is not FSharp.Core-only syntax, the parser Fantomas
  vendors is behind the compiler, which is a bug: report it with `--compiles` on the `issue` command.

If main passes while the latest release fails, the fix is on main and ships with the next release.

## 5. Look for related issues

`gh issue list -R fsprojects/fantomas --state all --search "<key words>"`, or the issue search on
GitHub without `gh`. Check the open ones and the recently closed ones.

Report anyway when one looks the same. The contribution guidelines ask for a new issue every time,
because bugs that look alike often have different causes and the maintainers are the ones who can
tell. Mention the issue that looks related in the description, and leave the call to them.

## 6. Open the issue form

Write the problem description to a file: what goes wrong, which variations trigger it and which do
not, the versions it fails on, and where the code came from. Keep it short. Then:

```bash
bun report.ts issue <sample> --title "<what goes wrong>" --description <file> [--fsi] [--setting ...]
```

It writes the body to `fantomas-issue-body.md` in the temp folder, prints the link length, and opens
the form. "The formatted result breaks my code" is ticked when the output does not parse; the other
boxes are left for the user. A body too long for a link opens the form with the title only and puts
the body on the clipboard. When that happens, first try to shrink the sample further.

Tell the user what was opened, and that nothing is filed until they submit it.
