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

	dotnet new tool-manifest

Install the command line tool with:

	dotnet tool install fantomas

or install the tool globally with

	dotnet tool install -g fantomas

## Usage

For the overview how to use the tool, you can type the command

	dotnet fantomas --help
*)
(*** hide ***)
open System.Diagnostics

let fantomasDll =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../artifacts/bin/Fantomas/release/fantomas.dll")

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
The tool will explore the input folder recursively.
If you omit the output path, Fantomas will overwrite the input files unless the content did not change.


### JSON output

*starting version 8.0*

`--json` writes one JSON document to standard out describing what the run did, instead of the
usual messages. It is meant for a script or an agent that has to act on the result rather than
read it:

```bash
dotnet fantomas --json ./src
```

```json
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
```

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

```json
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
```

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

```powershell
# Filter all added and modified files in git
# A useful function to add to your $PROFILE
function Format-Changed(){
    $files =
        git status --porcelain `
        | Where-Object { ($_.StartsWith(" M", "Ordinal") -or $_.StartsWith("AM", "Ordinal")) `
        -and (Test-FSharpExtension $_) } | ForEach-Object { $_.substring(3) }
    & "dotnet" "fantomas" $files
}
```

Or usage with `find` on Unix:

```bash
find my-project/ -type f -name "*.fs" -not -path "*obj*" | xargs dotnet fantomas --check
```

<fantomas-nav previous="../index.html" next="{{fsdocs-next-page-link}}"></fantomas-nav>
*)
