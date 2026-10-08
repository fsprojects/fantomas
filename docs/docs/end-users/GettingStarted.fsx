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

`files` is every file the run looked at. `status` is one of `formatted`, `unchanged`,
`needs-formatting`, `timed` or `error`, and which of them can appear depends on `command`, which is
`format`, `check` or `profile`: a check writes nothing, so it reports `needs-formatting` where a
format run reports `formatted`, and only a profile run reports `timed`.

A file that a `.fantomasignore` matched is not listed and is not counted anywhere either. There is
no honest number for it: a pattern that names a file can be counted, and one that names a folder
cannot, because the folder is never opened and what is inside it is as unknown as what is inside a
folder that is not there. `fantomas doctor <file>` is what answers that question about a path you
name, and it answers it exactly.

A file's `path` is the one you gave, so it is usually relative. `workingDirectory` is what it is
relative to, and the absolute path is the two joined. They are apart rather than resolved per file
so that a run over a thousand files does not repeat the same prefix a thousand times.

A file with status `error` carries three more keys. `missingComments` is only filled when Fantomas
refused its own output because it could not find comments of the file in it. Each is the comment's `text` as the
file has it and its `range` in the file. When Fantomas refused its output for not being valid F#,
each of the `diagnostics` also has `reportedUnder`: the define combinations of the output that
reported it, an empty one being the combination without defines. A file `--force` wrote although Fantomas refused
its output has status `formatted`, `"forced": true`, and the same `diagnostics` and
`missingComments`. A run where one file could not be parsed reports the whole thing like this:

```json
{
  "command": "format",
  "workingDirectory": "/home/you/my-project",
  "exitCode": 1,
  "error": null,
  "files": [
    { "path": "./src/App.fs", "status": "formatted" },
    {
      "path": "./src/Broken.fs",
      "status": "error",
      "message": "./src/Broken.fs could not be parsed by Fantomas",
      "diagnostics": [
        {
          "severity": "error",
          "code": "FS0583",
          "message": "Unmatched '('",
          "range": { "startLine": 3, "startColumn": 9, "endLine": 3, "endColumn": 10 }
        }
      ],
      "missingComments": []
    }
  ]
}
```

Lines and columns are both one based, the way the F# compiler prints them. Note that the top level
`error` is still `null` here: it is not where a file's failure is reported, but what stopped the run
before it reached any file at all, such as an input path that does not exist. The other files are
formatted as usual, and the run ends with exit code 1.

The document carries no version, and that is the promise rather than an omission. A version number
says a shape is a contract somebody is maintaining, and this one is not: it exists so that a machine
reading a run can see what happened, which is a job that tolerates the shape moving. What is written
here may change in any release. The exit code is the part that is promised.

`--json` cannot be combined with `--daemon`, where standard out already carries the JSON-RPC
protocol.

### Diagnosing one file

*starting version 8.0*

`doctor` walks one file through everything Fantomas does to it and reports what happened at each
step. It writes nothing, so it is safe against a working tree you have not committed, and it is
what to reach for when Fantomas did something to a file you did not expect, or did nothing to a
file you expected it to touch.

```bash
dotnet fantomas doctor src/App.fs
```

```
Fantomas 8.0.0+8f4c2b1a9 on /home/you/my-project/src/App.fs

+ File        Found on disk: an implementation file of 214 lines.
+ Ignore      Governed by /home/you/my-project/.fantomasignore, and no pattern in it matches.
+ Settings    2 of 36 settings come from /home/you/my-project/.editorconfig and
              /home/you/my-project/src/.editorconfig, the rest are Fantomas defaults.

              max_line_length = 100                        /home/you/my-project/.editorconfig
              fsharp_multiline_bracket_style = stroustrup  /home/you/my-project/src/.editorconfig

              end_of_line = lf                             the Fantomas default
              indent_size = 4                              the Fantomas default
              insert_final_newline = true                  the Fantomas default
              ...
+ Parse       Fantomas parsed your file.
+ Format      Fantomas formatted your file.
+ Valid       The formatted result is valid F#.
+ Comments    The formatted result keeps every comment and directive of your file.
+ Idempotent  Formatting the formatted result again changes nothing.
! Verdict     Fantomas would write the formatted result to your file, which changes it first at line 12.
```

The opening line carries the whole version, commit hash and all, where every other page trims it to
the short form. This report is what gets pasted into a bug report, and the build that produced it is
the first thing whoever reads it has to know.

The steps are the ones Fantomas takes, in the order it takes them, and each one gates the next:

* **File**: is there a file at that path at all, and is it one Fantomas formats? A `.fsx` is
  named as a script and a `.fsi` as a signature file, since which of the three it is decides how
  Fantomas parses it. A file under a folder a compiler
  or a package manager wrote, such as `obj`, is named as such: a run over the tree above it never
  opens that folder, so the file is invisible to it however the ignore file is written.
* **Ignore**: which `.fantomasignore` governs the file, and which line of it decided, quoted with
  its line number. Only the nearest one at or above the file applies; unlike `.gitignore`, Fantomas
  does not merge in the ones above it. A file an ignore file matches stops the walk here, because
  that is where Fantomas stops with it too.
* **Settings**: every setting the file will be formatted with, and for each one that an
  `.editorconfig` set, which file set it. What an `.editorconfig` decided comes first, then a blank
  line, then everything left at its Fantomas default. The line above them names the files that set
  something, which is not always every file in the chain. Anything Fantomas could not use out of
  the chain is reported here too, below both.
* **Parse**: whether the file is valid F#. A file with conditional directives is parsed once for
  each combination of its defines, and the combinations are listed under the row: they are what
  the defines named further down refer to. A file that will not parse fails here, with the parser's
  diagnostics and a snippet under the row, or with the combinations it fails under.
* **Format**: whether Fantomas could format the file.
* **Valid**: whether the formatted result is valid F#. It always should be; when it is not, what
  the parser said about it is under the row, and that is a bug in Fantomas rather than a problem
  with the file.
* **Comments**: whether the result keeps every `//` and `(* *)` comment, conditional directive and
  warn directive of the file. It always should. It runs the search a format run does, for the text
  of every comment in the order of the file, which is what a format run refuses a result over. It
  also compares the comments and directives of result and file under every define combination,
  which is where a comment printed twice or a changed directive shows up; a format run does not check
  that, and writes such a result. What the result lost, gained or rewrote is listed under the row.
  XML doc comments (`///`) are not checked.
* **Idempotent**: whether formatting the formatted result again leaves it alone. It should, and
  when it does not the file keeps changing under a formatter that is run twice.
* **Verdict**: what a format run would do with the file, given the steps above: leave it as it is
  when it is already formatted, write the formatted result, or leave the file unchanged because the
  result is not valid F# or Fantomas cannot find a comment of the file in it, the two things a
  format run refuses a result over.
  Whether the file is already formatted is decided the way a format run decides it, by comparing
  the text as it is, so a file whose line endings are the only thing out of step gets written.

A step the walk never reached is named below the table with the reason it was not looked at, rather
than left out or shown as having found nothing. When the formatted result is not valid F#, the
Comments step still says which comments a format run would not find, and Idempotent is not looked
at: Fantomas does not format output with errors again. When any step found a bug in Fantomas,
the report ends by asking for it to be reported, once.

`doctor` takes one file rather than a folder, because the answers differ per file and a table per
file is not a report. It exits 0 for a file it could diagnose, whatever it found, and 1 when the
path is not a file it can look at or when a step failed: a file that will not format, output
Fantomas will not accept, output that does not keep every comment, or a second pass that changed
the first. A file that needs formatting is not a failure; `fantomas check` is what fails over that.

`--json` writes the same walk as one document, with a key per step and `null` where the walk
stopped before reaching it. The verdict has no key: it is read off the steps. The `configuration` key carries every setting, with `setBy` naming the
file for each one an `.editorconfig` set and `null` for the rest.

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

<fantomas-nav source="{{fsdocs-source-filename}}" previous="../index.html" next="{{fsdocs-next-page-link}}"></fantomas-nav>
*)
