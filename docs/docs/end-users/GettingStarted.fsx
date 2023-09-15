(**
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
*)
(*** hide ***)
open System.Diagnostics

let fantomasDll =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../src/Fantomas/bin/Release/net6.0/fantomas.dll")

let output =
    let psi = ProcessStartInfo("dotnet", $"{fantomasDll} --help")
    psi.RedirectStandardOutput <- true
    psi.UseShellExecute <- false
    let p = Process.Start(psi)
    let reader = p.StandardOutput
    let result = reader.ReadToEnd()
    p.WaitForExit()
    result

printfn $"%s{output}"
(*** include-output  ***)

(**
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

<fantomas-nav previous="{{fsdocs-previous-page-link}}" next="{{fsdocs-next-page-link}}"></fantomas-nav>
*)
