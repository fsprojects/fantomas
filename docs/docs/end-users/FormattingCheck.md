---
category: End-users
categoryindex: 1
index: 5
---

# Formatting Check

*starting version 3.3*

Verify that a single file or folder was formatted correctly.

> dotnet fantomas --check Source.fs

This will verify if the file `Source.fs` still needs formatting.
If it does, the process will return exit code 99.
In the case that the file does not require any formatting, exit code 0 is returned.
Unexpected errors will return exit code 1.

This scenario is meant to be executed in a continuous integration environment, to enforce that the newly added code was formatted correctly.


## Set files to be formatted inside build.fsx with snippet
## Add format param to build.fsx with snippet
## Add CheckFormat param to build.fsx with snippet
## How to run