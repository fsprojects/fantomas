---
category: End-users
categoryindex: 1
index: 18
---
# Reporting a bug

Fantomas should never break your code. When it does, when it throws, or when formatting its own result
changes it again, we would like to hear about it.

## The online tool

We prefer bugs to be reported through the [online tool](https://fsprojects.github.io/fantomas-tools/#/fantomas/main).
Paste the code, pick the settings you use, and press *Looks wrong? Create an issue!*. It formats with
the latest main branch and opens a GitHub issue with everything a maintainer needs to reproduce it.

Keep the code as small as you can. Isolate the part that goes wrong: a report of five lines gets
fixed much sooner than a file of five hundred. It also has to fit in a link, and GitHub refuses a link
to a new issue once it grows past a few thousand characters.

Not sure which settings apply to a file? `dotnet fantomas doctor <file>` lists every setting your
`.editorconfig` sets for it, see [Diagnosing one file](GettingStarted.html#Diagnosing-one-file).

Please report a new issue even when one looks the same. Bugs that look alike often have different
causes, and the maintainers can tell.

## With a coding agent

If you work with a coding agent, the `fantomas-report` skill does the same, starting from the file
Fantomas failed on:

```bash
npx skills add fsprojects/fantomas --skill fantomas-report -g
```

Then ask your agent to report the file Fantomas could not format. The skill:

* shrinks the code to a minimal sample, and notes which variations make a difference;
* tries it against released versions, to tell a regression from an old bug, and a bug that is already
  fixed from one that is not;
* recognizes code Fantomas cannot format by design, such as the
  [nesting that depends on a define](ConditionalCompilationDirectives.html#Nesting-that-depends-on-a-define),
  and proposes a change to your code instead;
* checks the sample against the latest main branch, the same way the online tool does;
* opens the new issue form in your browser, filled in the way the online tool fills it.

It never submits the issue: you read it and decide. It needs the .NET 10 SDK, and
[Bun](https://bun.sh) or Node.js 22.18 or later. It asks before it clones the Fantomas repository,
which makes shrinking a sample much faster.

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
