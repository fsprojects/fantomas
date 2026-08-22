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
Fantomas is an opinionated source code formatter for F#. (8.0.0-alpha-014+e628e02cb)

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
      --daemon               Run an LSP-like server that editor tooling can talk to.
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