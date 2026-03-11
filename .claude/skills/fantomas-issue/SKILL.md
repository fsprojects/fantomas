---
description: Investigate and fix a Fantomas formatting issue from GitHub
---

The input is a Fantomas GitHub issue URL (e.g. https://github.com/fsprojects/fantomas/issues/1234).

Follow these steps in order:

## 1. Fetch the issue

Use `gh issue view <number> --repo fsprojects/fantomas --json title,body,labels` to get the issue details. Extract the example code and expected behavior. Note the labels — `bug (soundness)` vs `bug (stylistic)` affects the changelog entry.

## 2. Reproduce the problem

Use the /format skill to format the example code and confirm the bug exists. If confirmed, try to trim the example down to the minimal reproduction case.

## 3. Add a failing unit test

Find a suitable test file in `src/Fantomas.Core.Tests/`. Look for existing tests related to the same AST node or concept using grep. If no good file exists, ask the user where to create a new file.

A new test file should follow this template:

```fsharp
module Fantomas.Core.Tests.MyNewConceptTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// add tests here...
```

Test naming rules:
- Test names must start with a **lowercase** letter.
- When linked to a GitHub issue, add the issue number at the back with a comma: `` let ``my test description, 1234`` () = ``
- You don't need to repeat the issue number for tests that are variations of the original report.

### Verify signature files

Check if the fix should also apply to signature files (`*.fsi`). If so, add a test using `formatSignatureString` or the `--signature` flag.

### Verify slight variations

- Check if additional tests are needed for different setting combinations.
- If the code involves `#if`/`#else` directives, use `formatSourceStringWithDefines` to test each define combination separately, plus a `formatSourceString` test for the merged result. Name suffixes: `, no defines`, `, DEBUG`, `, 1234`.

Run the test and **assert it fails** before proceeding to the fix.

## 4. Investigate the root cause

Use the /ast, /oak, and /writer-events skills to understand what's happening. Key files to inspect:
- `src/Fantomas.Core/CodePrinter.fs` — the main printer
- `src/Fantomas.Core/Context.fs` — writer context
- `src/Fantomas.Core/ASTTransformer.fs` — AST to Oak transformation
- `src/Fantomas.Core/Trivia.fs` — trivia (comments, blank lines, directives)

## 5. Implement the fix

Make the minimal change needed. Run the new test to confirm it passes.

## 6. Run all tests

Run `dotnet test src/Fantomas.Core.Tests/Fantomas.Core.Tests.fsproj`. If many tests fail, the fix is likely too broad — make it more targeted. If only a few tests fail and the new behavior is arguably better, update those tests and ask the user for their opinion (a git diff is easiest to review).

## 7. Update CHANGELOG.md

Add an entry under the `## [Unreleased]` section. Never add to an already-published version section. If there is no `Unreleased` section, create one at the top above the most recent version.

- For `bug (soundness)` fixes, add under `### Fixed` using the original issue title:
  `- <Original GitHub issue title>. [#1234](https://github.com/fsprojects/fantomas/issues/1234)`
- For `bug (stylistic)` fixes (not related to a style guide), also add under `### Fixed`.
- For `bug (stylistic)` fixes related to a style guide, add under `### Changed`:
  `- Update style of xyz. [#1234](https://github.com/fsprojects/fantomas/issues/1234)`

## 8. Run analyzers

Run `dotnet msbuild /t:AnalyzeSolution` to check for analyzer warnings/errors.

## 9. Format edited files

Run `dotnet fantomas <file>` on all `.fs` and `.fsx` files you edited to ensure they conform to the project's formatting standard.
