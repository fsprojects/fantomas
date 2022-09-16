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

> dotnet new tool-manifest

Install the command line tool with:

> dotnet tool install fantomas

or install the tool globally with
> dotnet tool install -g fantomas

## Usage

For the overview how to use the tool, you can type the command

	dotnet fantomas --help

```
USAGE: dotnet fantomas [--help] [--recurse] [--force] [--profile] [--fsi <string>] [--stdin] [--stdout] [--out <string>] [--check] [--daemon] [--version] [<string>...]

INPUT:

    <string>...           Input paths: can be multiple folders or files with *.fs,*.fsi,*.fsx,*.ml,*.mli extension.

OPTIONS:

    --recurse, -r         Process the input folder recursively.
    --force               Print the source unchanged if it cannot be parsed correctly.
    --profile             Print performance profiling information.
    --fsi <string>        Read F# source from stdin as F# signatures.
    --stdin               Read F# source from standard input.
    --stdout              Write the formatted source code to standard output.
    --out <string>        Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only.
    --check               Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error
    --daemon              Daemon mode, launches an LSP-like server to can be used by editor tooling.
    --version, -v         Displays the version of Fantomas
    --help                display this list of options.

```

You have to specify an input path and optionally an output path. 
The output path is prompted by `--out` e.g.

	dotnet fantomas ./input/array.fs --out ./output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option.
If you omit the output path, Fantomas will overwrite the input files unless the content did not change.


### Multiple paths

*starting version 4.5*

Multiple paths can be passed as last argument, these can be both files and folders.  
This cannot be combined with the `--out` flag.  
When combined with the `--recurse` flag, all passed folders will be processed recursively.

One interesting use-case of passing down multiple paths is that you can easily control the selection and filtering of paths from the current shell.

Consider the following PowerShell script:

```powershell
# Filter all added and modified files in git
# A useful function to add to your $PROFILE
function Format-Changed(){
    $files = 
        git status --porcelain `
        | Where-Object { ($_.StartsWith(" M") -or $_.StartsWith("AM")) `
        -and (Test-FSharpExtension $_) } | ForEach-Object { $_.substring(3) }
    & "dotnet" "fantomas" $files
}
```

Or usage with `find` on Unix:

```bash
find my-project/ -type f -name "*.fs" -not -path "*obj*" | xargs dotnet fantomas --check
```

<fantomas-nav previous="../index.html" next="./StyleGuide.html"></fantomas-nav>
