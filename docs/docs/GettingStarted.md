---
category: End-user documentation
categoryindex: 1
index: 1
---
# Getting Started

## Quick install

* Command line: `dotnet tool install -g fantomas`
* JetBrains Rider: preinstalled
* VSCode: part of [Ionide](http://ionide.io/) and [fantomas-fmt](https://marketplace.visualstudio.com/items?itemName=paolodellepiane.fantomas-fmt).
* [Visual Studio 2019](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs)
* [Visual Studio 2022](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs22)

## How to use

### Command line tool / API

Create a [.NET tool manifest](https://docs.microsoft.com/en-us/dotnet/core/tools/local-tools-how-to-use) to install tools locally
> dotnet new tool-manifest

Install the command line tool with:
> dotnet tool install fantomas

or install the tool globally with
> dotnet tool install -g fantomas

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

	dotnet fantomas ../../../../tests/stackexchange/array.fs --out ../../../../tests/stackexchange_output/array.fs 

Both paths have to be files or folders at the same time. 
If they are folders, the structure of input folder will be reflected in the output one. 
The tool will explore the input folder recursively if you set `--recurse` option.
If you omit the output path, Fantomas will overwrite the input files.

### Check mode

*starting version 3.3*

Verify that a single file or folder was formatted correctly.

> dotnet fantomas --check Source.fs

This will verify if the file `Source.fs` still needs formatting.
If it does, the process will return exit code 99.
In the case that the file does not require any formatting, exit code 0 is returned.
Unexpected errors will return exit code 1.

This scenario is meant to be executed in a continuous integration environment, to enforce that the newly added code was formatted correctly.

### Multiple paths

*starting version 4.5*

Multiple paths can be passed as last argument, these can be both files and folders.  
This cannot be combined with the `--out` and `--stdout` flags.  
When combined with the `--recurse` flag, all passed folders will be processed recursively.

One interesting use-case of passing down multiple paths is that you can easily control the selection and filtering of paths from the current shell.

Consider the following PowerShell scripts:
```powershell
# Create an array with paths
$files =
     Get-ChildItem src/*.fs -Recurse # Find all *.fs files in src,
     | Where-Object { $_.FullName -notlike "*obj*" } # ignore files in the `obj` folder
     | ForEach-Object { $_.FullName } #  and select the full path name.

& "dotnet" "fantomas" $files
```

```powershell
# Filter all added and modified files in git
$files = git status --porcelain | Where-Object { $_ -match "^\s?A?M(.*)\.fs(x|i)?$" } | ForEach-Object { $_.TrimStart("AM").TrimStart(" ", "M") }
& "dotnet" "fantomas" $files
```

Or usage with `find` on unix:

`find my-project/ -type f -name "*.fs" -not -path "*obj*" | xargs dotnet fantomas --check`

### FAKE build system
Fantomas can be easily integrated with FAKE build system.<br />
Check out the [sample](https://github.com/fsprojects/fantomas/blob/master/fake-sample/README.md).

### JetBrains Rider

The [resharper-fsharp](https://github.com/JetBrains/resharper-fsharp) uses fantomas under the hood to format the source code. No need for any additional plugins.

#### Using the latest version inside Rider

For technical reasons Rider cannot always use the latest version of Fantomas found on NuGet.
As a workaround you could install [fantomas](https://www.nuget.org/packages/fantomas-tool) locally with `dotnet tool install fantomas-tool` and configure it as an [External tool](https://www.jetbrains.com/help/rider/Settings_Tools_External_Tools.html).

![Rider external tool window](./docs/rider-external-tool.png)

![Rider action window](./docs/rider-action-window.png)

**This will have an impact on your editing experiencing in Rider**, the external change to the file by the command line application might trigger more internal logic inside Rider than necessary.
It could be noticeable in regards to the default formatting experience.

### Visual Studio Code

The recommended way to use Fantomas is by using the [Ionide plugin](http://ionide.io/). Fantomas is integrated in [FSAutoComplete](https://github.com/fsharp/FsAutoComplete/) which is the language server used by Ionide.

Alternatively, you can install the [fantomas-fmt](https://marketplace.visualstudio.com/items?itemName=paolodellepiane.fantomas-fmt) extension.

### Visual Studio

The F# Formatting extension sets up Fantomas as the default formatter for F# files, configurable from Visual Studio's options.

* [Visual Studio 2019](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs)
* [Visual Studio 2022](https://marketplace.visualstudio.com/items?itemName=asti.fantomas-vs22)

### Visual Studio for Mac

Install [fantomas](https://www.nuget.org/packages/fantomas) locally with `dotnet tool install fantomas-tool` and configure it as an [External tool]

![VS Mac external tool window](./docs/vsmac-external-tool.png)
### Online

Try the Fantomas [online](https://fsprojects.github.io/fantomas-tools/#/fantomas/preview).
