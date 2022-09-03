---
category: End-users
categoryindex: 1
index: 4
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

Note that Fantomas only searches for a `.fantomasignore` file in or above its current working directory, if one exists; unlike Git, it does not traverse the filesystem for each input file to find an appropriate ignore file.
(This is not true of the Fantomas daemon. The daemon can't rely on being invoked from the right place, and indeed there may not even be a well-defined notion of "right place" for the formatting tasks the daemon is required to perform, so it does search the filesystem for every file individually.)

Also note that if you are less familiar with `.gitignore`, `.gitgnore` processes everything using Unix slashes `/`.  
Windows slashes ` \ ` will not work correctly. See [official Git documentation](https://git-scm.com/docs/gitignore#_pattern_format) for more info.

## Great for gradual adoption

It is not always possible to format all code from the moment you start using Fantomas. Your team might be working on a lot of features and the initial format can lead to a hugh set of changes in source control.  
The `.fantomasignore` file can help you to introduce Fantomas **bit by bit** to a new code base.  
A good example of this is [dotnet/fsharp](https://github.com/dotnet/fsharp/blob/main/.fantomasignore), the maintainers initially only formatted signature files and are formatting more code over time.

## A storm in a teacup

Fantomas is not perfect, there are open issues and depending on what shenanigans you have in your code you might at some point encounter a bug 😅🙈.  
Before you've decided that Fantomas is not for you, you might want to use a `.fantomasignore` file to overcome that one problem.
In the past people have been quick to judge that the tool cannot be used, however, through a different looking glass Fantomas maybe did format *99%* of your code correctly.

<fantomas-nav previous="./Configuration.html" next="./FormattingCheck.html"></fantomas-nav>
