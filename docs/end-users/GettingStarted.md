---
category: End-users
categoryindex: 1
index: 1
---

# Getting Started

Fantomas should be installed as a [.NET tool](https://docs.microsoft.com/en-us/dotnet/core/tools/global-tools).
It is recommended to install it as a local tool and stick to a certain version per repository.

## Installation

Create a [.NET tool manifest](https://docs.microsoft.com/en-us/dotnet/core/tools/local-tools-how-to-use) to install tools locally.
You can skip this step if you wish to install Fantomas globally.

```fsharp
dotnet new tool-manifest

```

Install the command line tool with:

```fsharp
dotnet tool install fantomas

```

or install the tool globally with

```fsharp
dotnet tool install -g fantomas

```

## Usage

For the overview how to use the tool, you can type the command

```fsharp
dotnet fantomas --help

```

```
Fantomas is an opinionated source code formatter for F#. (8.0.0-alpha-015+cfd315e95)

Usage: fantomas [...flags] [...paths]

Examples:
  fantomas .                     Format every F# file below the current folder
  fantomas src/App.fs            Format a single file in place
  fantomas --check .             Report what needs formatting, write nothing
  fantomas --out build src       Copy the formatted files to another folder

Flags:
      --check                Report which files need formatting and write nothing.
                             Exits 0 when every file is already formatted, 99 when some
                             file needs formatting, and 1 when an error occurred.
      --out <path>           Write the result to this file or folder instead of formatting
                             in place. Takes a single input path.
      --force                Write the output even when it is not valid F# code.
                             For debugging purposes only.
      --profile              Print the line count and the time taken for every file.
      --json                 Report what the run did as one JSON document on standard out,
                             naming every file and positioning what went wrong. The usual
                             messages are not printed; warnings go to standard error.
      --daemon               Run an LSP-like server that editor tooling can talk to.
                             Takes no other flags or paths, apart from --verbosity.
  -v, --verbosity <level>    How much to print: normal or detailed. Defaults to normal.
                             n and d are accepted as well.
      --version              Print the version and exit
  -h, --help                 Display this menu and exit

Paths:
  A path is a folder, which is searched recursively, or a file ending in .fs, .fsi,
  .fsx, .ml or .mli. Formatting settings are read from .editorconfig, and files
  matched by .fantomasignore in the current folder are skipped.

Learn more about Fantomas:   https://fsprojects.github.io/fantomas/docs
Configure Fantomas:          https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html
Join the F# Discord:         https://discord.com/channels/196693847965696000/1493226271767924747
Docs for your LLM:           https://fsprojects.github.io/fantomas/llms.txt
                             https://fsprojects.github.io/fantomas/llms-full.txt
```

You have to specify an input path and optionally an output path.
The output path is prompted by `--out` e.g.

```fsharp
dotnet fantomas ./input/array.fs --out ./output/array.fs

```

Both paths have to be files or folders at the same time.
If they are folders, the structure of input folder will be reflected in the output one.
The tool will explore the input folder recursively.
If you omit the output path, Fantomas will overwrite the input files unless the content did not change.

### JSON output

*starting version 8.0*

`--json` writes one JSON document to standard out describing what the run did, instead of the
usual messages. It is meant for a script or an agent that has to act on the result rather than
read it:

dotnet fantomas --json ./src

{
  "version": 1,
  "command": "format",
  "workingDirectory": "/home/you/my-project",
  "exitCode": 0,
  "error": null,
  "files": [
    { "path": "./src/App.fs", "status": "formatted" },
    { "path": "./src/Library.fs", "status": "unchanged" }
  ]
}

Standard out carries the document and nothing else, so it can be piped straight into a parser.
Warnings, such as an `.editorconfig` setting Fantomas does not know, still go to standard error.
The exit code is unchanged from a run without the flag, and is in the document as well, so a
caller that captured the output has it either way.

Both commands list every file they looked at. `status` is one of `formatted`, `unchanged`,
`ignored`, `needs-formatting` or `error`. Which of them can appear depends on `command`, which is
`format` or `check`: a check writes nothing, so it reports `needs-formatting` where a format run
reports `formatted`.

A file's `path` is the one you gave, so it is usually relative. `workingDirectory` is what it is
relative to, and the absolute path is the two joined. They are apart rather than resolved per file
so that a run over a thousand files does not repeat the same prefix a thousand times.

A file with status `error` carries two more keys, and no other file does. A run where one file
could not be parsed reports the whole thing like this:

{
  "version": 1,
  "command": "format",
  "workingDirectory": "/home/you/my-project",
  "exitCode": 1,
  "error": null,
  "files": [
    { "path": "./src/App.fs", "status": "formatted" },
    {
      "path": "./src/Broken.fs",
      "status": "error",
      "message": "Fantomas could not parse ./src/Broken.fs",
      "diagnostics": [
        {
          "severity": "error",
          "code": "FS0583",
          "message": "Unmatched '('",
          "range": { "startLine": 3, "startColumn": 9, "endLine": 3, "endColumn": 10 }
        }
      ]
    }
  ]
}

Lines and columns are both one based, the way the F# compiler prints them. Note that the top level
`error` is still `null` here: it is not where a file's failure is reported, but what stopped the run
before it reached any file at all, such as an input path that does not exist. The other files are
formatted as usual, and the run ends with exit code 1.

`version` is the version of the document itself. It goes up when a key changes meaning or leaves,
not when one is added, so a reader that ignores what it does not recognise keeps working.

`--json` cannot be combined with `--daemon`, where standard out already carries the JSON-RPC
protocol.

### Multiple paths

*starting version 4.5*

Multiple paths can be passed as last argument, these can be both files and folders.
This cannot be combined with the `--out` flag.

One interesting use-case of passing down multiple paths is that you can easily control the selection and filtering of paths from the current shell.

Consider the following PowerShell script:

# Filter all added and modified files in git
# A useful function to add to your $PROFILE
function Format-Changed(){
    $files =
        git status --porcelain `
        | Where-Object { ($_.StartsWith(" M", "Ordinal") -or $_.StartsWith("AM", "Ordinal")) `
        -and (Test-FSharpExtension $_) } | ForEach-Object { $_.substring(3) }
    & "dotnet" "fantomas" $files
}

Or usage with `find` on Unix:

find my-project/ -type f -name "*.fs" -not -path "*obj*" | xargs dotnet fantomas --check

<fantomas-nav previous="../index.html" next="StyleGuide.md"></fantomas-nav>