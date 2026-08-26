---
category: End-users
categoryindex: 1
index: 5
---

# Ignore Files

*starting version 4.1*

To exclude files from formatting, create a `.fantomasignore` file in the root of your project.  
`.fantomasignore` uses [gitignore](https://git-scm.com/docs/gitignore) syntax (processed via [Ignore](https://github.com/goelhardik/ignore)).  
Ignored files will be picked up by the [Fantomas command line tool](https://www.nuget.org/packages/fantomas/).

Exclusion applies both to formatting and the format checking.

```
# Ignore Fable files
.fable/

# Ignore script files
*.fsx
```

Note that the `.fantomasignore` that governs a file is the nearest one at or above that file, and only that one. Unlike Git, Fantomas does not merge in the ignore files above it, so a pattern in a parent repository's ignore file has no effect on a file that has one of its own beside it.

*starting version 8.0*, the command line resolves this per file, which is what the daemon has always done. Before that it resolved a single ignore file for the whole run from the directory it was started in, so an ignore file in a subfolder was honoured by an editor and invisible to a pipeline.

If you are not sure which ignore file governs a given source file, or which line of it decided, ask:

```bash
dotnet fantomas doctor src/App.fs
```

It names the ignore file, quotes the line that matched with its line number, and writes nothing. Where a pattern in an ignore file further up would have skipped the file, it names that file too and quotes the pattern, which is the answer when something you wrote at the root of a repository turns out to have had no effect. See [Getting Started](./GettingStarted.html) for the rest of what it reports.

Also note that if you are less familiar with `.gitignore`, `.gitgnore` processes everything using Unix slashes `/`.  
Windows slashes `\` will not work correctly. See [official Git documentation](https://git-scm.com/docs/gitignore#_pattern_format) for more info.

## Great for gradual adoption

It is not always possible to format all code from the moment you start using Fantomas. Your team might be working on a lot of features and the initial format can lead to a hugh set of changes in source control.  
The `.fantomasignore` file can help you to introduce Fantomas **bit by bit** to a new code base.  
A good example of this is [dotnet/fsharp](https://github.com/dotnet/fsharp/blob/main/.fantomasignore), the maintainers initially only formatted signature files and are formatting more code over time.

## A storm in a teacup

Fantomas is not perfect, there are open issues and depending on what shenanigans you have in your code you might at some point encounter a bug 😅🙈.  
Before you've decided that Fantomas is not for you, you might want to use a `.fantomasignore` file to overcome that one problem.
In the past people have been quick to judge that the tool cannot be used, however, through a different looking glass Fantomas maybe did format *99%* of your code correctly.

<fantomas-nav previous="Configuration.md" next="FormattingCheck.md"></fantomas-nav>