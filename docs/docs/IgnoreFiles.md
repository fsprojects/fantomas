---
category: End-user documentation
categoryindex: 1
index: 3
---
# Ignore Files 

*starting version 4.1*

To exclude files from formatting, create a `.fantomasignore` file in the root of your project.
`.fantomasignore` uses [gitignore](https://git-scm.com/docs/gitignore) syntax (processed via [Ignore](https://github.com/goelhardik/ignore)).
Ignored files will be picked up when the [Fantomas cli tool](https://www.nuget.org/packages/fantomas/).
Exclusion applies both to formatting and the format checking.

```
# Ignore Fable files
.fable/

# Ignore script files
*.fsx
```

Note that Fantomas only searches for a `.fantomasignore` file in or above its current working directory, if one exists; unlike Git, it does not traverse the filesystem for each input file to find an appropriate ignore file.
(This is not true of the Fantomas daemon. The daemon can't rely on being invoked from the right place, and indeed there may not even be a well-defined notion of "right place" for the formatting tasks the daemon is required to perform, so it does search the filesystem for every file individually.)
