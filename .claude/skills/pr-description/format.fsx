// Copied by build.fsx into a built tree as scripts/pr-description-format.fsx, beside the tree's own
// shared.fsx, so each side formats with its own build and reads its own flags.
//
// Formats the way the tool does, without the snapshot harness: the harness stops on its internal
// checks, which is not what a user saw. Prints the result on stdout, and on stderr whether the result
// passes the validation the tool runs before writing, and whether a second format leaves it alone.
// That validation parses every define combination and reports the first that fails: its errors,
// and the warnings Fantomas does not tolerate (such as FS0058, offside), on code that may parse.
//
// It needs CodeFormatter.ValidateFSharpCodeAsync, which trees from 2026-08-25 on have; measure an
// older tree in compare.fsx's cli mode. From v9.0 on, FormatDocumentWithValidationsAsync runs the
// checks the tool itself runs, with FormatResult.Issues saying what they found: once both sides of a
// pull request have it, report those instead of checking here.
#load "shared.fsx"

open Fantomas.Core
open Shared

let source, isSignature, config, defines = parseArgs fsi.CommandLineArgs[1..]

// scripts/format.fsx prints one define combination with --define; users get every one merged.
if not defines.IsEmpty then
    eprintfn "--define is not supported here: users get every define combination merged."
    exit 1

let format (code: string) : string =
    (CodeFormatter.FormatDocumentAsync(isSignature, code, config)
     |> Async.RunSynchronously)
        .Code

// The check the tool makes before it writes a result: a result it refuses leaves the file unchanged.
let problemsOf (code: string) : string list =
    (CodeFormatter.ValidateFSharpCodeAsync(isSignature, code)
     |> Async.RunSynchronously)
        .Diagnostics
    |> List.map (fun d ->
        let number: string = d.ErrorNumber |> Option.map string |> Option.defaultValue "?"

        let position: string =
            match d.Range with
            | Some range -> $" (line %d{range.StartLine}, column %d{range.StartColumn})"
            | None -> ""

        $"FS%s{number}%s{position}: %s{d.Message}")

let first: string =
    try
        format source
    with ex ->
        eprintfn $"-- formatting failed: %s{ex.Message}"
        exit 1

printf $"%s{first}"

match problemsOf first with
| [] ->
    match
        (try
            Ok(format first)
         with ex ->
             Error ex.Message)
    with
    | Ok second when second = first -> eprintfn "-- valid, idempotent"
    | Ok second -> eprintfn $"-- valid, not idempotent; second pass:\n%s{second}"
    | Error message -> eprintfn $"-- valid, second pass failed: %s{message}"
| problems ->
    eprintfn "-- rejected by validation:"

    for problem in problems do
        eprintfn $"   %s{problem}"
