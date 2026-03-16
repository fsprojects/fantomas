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
Learn more about Fantomas:       https://fsprojects.github.io/fantomas/docs
Join our Discord community:      https://discord.gg/Cpq9vf8BJH

USAGE: dotnet fantomas [--help] [--force] [--profile] [--out <string>] [--check]
                       [--daemon] [--version] [--verbosity <string>]
                       [<string>...]

INPUT:

    <string>...           Input paths: can be multiple folders or files with
                          *.fs,*.fsi,*.fsx,*.ml,*.mli extension.

OPTIONS:

    --force               Print the output even if it is not valid F# code. For
                          debugging purposes only.
    --profile             Print performance profiling information.
    --out <string>        Give a valid path for files/folders. Files should
                          have .fs, .fsx, .fsi, .ml or .mli extension only.
                          Multiple files/folders are not supported.
    --check               Don't format files, just check if they have changed.
                          Exits with 0 if it's formatted correctly, with 1 if
                          some files need formatting and 99 if there was an
                          internal error
    --daemon              Daemon mode, launches an LSP-like server that can be
                          used by editor tooling.
    --version             Displays the version of Fantomas
    --verbosity, -v <string>
                          Set the verbosity level. Allowed values are n[ormal]
                          and d[etailed].
    --help                display this list of options.
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

**starting version 4.5**

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