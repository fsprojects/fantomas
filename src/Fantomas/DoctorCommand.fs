module Fantomas.DoctorCommand

open System
open System.IO.Abstractions
open Fantomas.Core
open Fantomas
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.EditorConfigFiles
open Fantomas.Paths

// Carriage returns are folded away before splitting, so a file written with the other platform's
// line endings has the same lines as one written with this platform's. A last line with no newline
// after it is still a line, and the newline that ends the last line does not start another.
let lines (content: string) : string array =
    if String.IsNullOrEmpty content then
        Array.empty
    else

    let split: string array = content.Replace("\r\n", "\n").Split('\n')

    if String.IsNullOrEmpty split.[split.Length - 1] then
        split.[.. split.Length - 2]
    else
        split

let firstDifference (left: string array) (right: string array) : int option =
    let shared: int = min left.Length right.Length

    let rec search (index: int) : int option =
        if index = shared then
            // Everything they both have is the same, so they differ only if one has more.
            if left.Length = right.Length then
                None
            else
                Some(shared + 1)
        elif left.[index] <> right.[index] then
            Some(index + 1)
        else
            search (index + 1)

    search 0

[<RequireQualifiedAccess; Struct>]
type FileKind =
    | Implementation
    | Signature
    | Script

[<RequireQualifiedAccess; NoComparison>]
type FileStep =
    | NotFound of path: string
    | NotFSharp of path: string
    | Candidate of file: DoctorFile

and [<NoComparison>] DoctorFile =
    {
        Path: string
        Kind: FileKind
        LineCount: int
        UnreachableUnder: string option
    }

[<RequireQualifiedAccess; NoComparison>]
type IgnoreStep =
    | NoIgnoreFile
    | Governed of ignoreFile: string * isIgnored: bool * matches: IgnoreMatch list * shadowed: ShadowedIgnoreFile list

and [<NoComparison>] ShadowedIgnoreFile =
    {
        Path: string
        WouldIgnore: bool
        Matches: IgnoreMatch list
    }

[<RequireQualifiedAccess; Struct>]
type FormatChange =
    | Nothing
    | LineEndingsOnly
    | Reformatted of firstChangedLine: int * lineCountAfter: int

[<RequireQualifiedAccess; NoComparison>]
type ParseStep =
    | Parsed of combinations: string list list
    | Failed of error: exn

[<RequireQualifiedAccess; NoComparison>]
type FormatStep =
    | Failed of error: exn
    | Produced of formatted: string * change: FormatChange

[<RequireQualifiedAccess; NoComparison>]
type ValidityStep =
    | Valid
    | Invalid of issues: Fantomas.Core.ValidationIssue list

[<RequireQualifiedAccess; NoComparison>]
type TriviaStep =
    | Kept
    | Changed of changes: ValidationIssue list

[<RequireQualifiedAccess; NoComparison>]
type IdempotencyStep =
    | Idempotent
    | NotIdempotent of line: int * afterFirstPass: string * afterSecondPass: string
    | Failed of error: exn

[<NoComparison>]
type DoctorReport =
    {
        File: FileStep
        Ignore: IgnoreStep option
        Settings: ResolvedConfig option
        Parse: ParseStep option
        Format: FormatStep option
        Validity: ValidityStep option
        Trivia: TriviaStep option
        Idempotency: IdempotencyStep option
    }

[<RequireQualifiedAccess; NoComparison>]
type DoctorCommandResult =
    | NotOneFile of given: InputPath
    | Completed of report: DoctorReport
    | Failed of error: exn

    member this.ExitCode: int =
        match this with
        | DoctorCommandResult.NotOneFile _
        | DoctorCommandResult.Failed _ -> 1
        | DoctorCommandResult.Completed report ->

        let stepFailed: bool =
            match report.File with
            | FileStep.NotFound _
            | FileStep.NotFSharp _ -> true
            | FileStep.Candidate _ ->
                let parseFailed: bool =
                    match report.Parse with
                    | Some(ParseStep.Failed _) -> true
                    | Some(ParseStep.Parsed _)
                    | None -> false

                let formatFailed: bool =
                    match report.Format with
                    | Some(FormatStep.Failed _) -> true
                    | Some(FormatStep.Produced _)
                    | None -> false

                let outputRefused: bool =
                    match report.Validity with
                    | Some(ValidityStep.Invalid _) -> true
                    | Some ValidityStep.Valid
                    | None -> false

                let triviaChanged: bool =
                    match report.Trivia with
                    | Some(TriviaStep.Changed _) -> true
                    | Some TriviaStep.Kept
                    | None -> false

                let secondPassDisagreed: bool =
                    match report.Idempotency with
                    | Some(IdempotencyStep.NotIdempotent _)
                    | Some(IdempotencyStep.Failed _) -> true
                    | Some IdempotencyStep.Idempotent
                    | None -> false

                parseFailed
                || formatFailed
                || outputRefused
                || triviaChanged
                || secondPassDisagreed

        if stepFailed then 1 else 0

/// A report that got as far as `file` and no further. Every step is asked for in order and the
/// ones that were never reached stay `None`, so building the record up this way is what keeps a
/// step that was skipped distinguishable from one that had nothing to say.
let stoppedAt (file: FileStep) : DoctorReport =
    {
        File = file
        Ignore = None
        Settings = None
        Parse = None
        Format = None
        Validity = None
        Trivia = None
        Idempotency = None
    }

/// The nearest folder above the file that a walk would refuse to open, when there is one.
///
/// `findAllFilesRecursively` never descends into a folder a compiler or a package manager wrote,
/// and it does that before any ignore file is consulted. So a file under `obj` is invisible to a
/// run over the tree above it, and sending its owner to read their `.fantomasignore` sends them to
/// read the wrong file.
///
/// The folder the walk itself was pointed at is not one of these: the rule is asked of the
/// subfolders a walk finds, so `fantomas obj` opens `obj` and this cannot know how the walk that
/// somebody is puzzled about was started. Naming the nearest excluded folder above the file is the
/// most that can be said without guessing.
let unreachableUnder (fs: IFileSystem) (file: string) : string option =
    let rec walkUp (directory: IDirectoryInfo) : string option =
        if isNull directory then
            None
        elif isExcludedDirName directory.Name then
            Some directory.FullName
        else
            walkUp directory.Parent

    walkUp (fs.FileInfo.New(file).Directory)

// Which extension means what, decided the way the rest of the tool decides it: `.fsi` and nothing
// else is parsed as a signature, so `.mli` comes back as an implementation here because that is
// what it is formatted as.
let fileKind (path: string) : FileKind =
    if path.EndsWith(".fsi", StringComparison.OrdinalIgnoreCase) then
        FileKind.Signature
    elif path.EndsWith(".fsx", StringComparison.OrdinalIgnoreCase) then
        FileKind.Script
    else
        FileKind.Implementation

let describeFile (fs: IFileSystem) (path: string) (content: string) : DoctorFile =
    {
        Path = path
        Kind = fileKind path
        LineCount = Array.length (lines content)
        UnreachableUnder = unreachableUnder fs path
    }

let askIgnore (env: CliEnvironment) (file: string) : IgnoreStep =
    match env.FindIgnoreFile file with
    | None -> IgnoreStep.NoIgnoreFile
    | Some ignoreFile ->

    // Each one above asked the same two questions the governing file is asked, so that what is
    // reported of it is what would have been reported had it been the one that applies. An
    // ignore file described in weaker terms than the one beside it reads as the lesser fact,
    // and which of the two governs is the whole point being made.
    let shadowed: ShadowedIgnoreFile list =
        env.FindIgnoreFilesAbove ignoreFile
        |> List.map (fun (above: IgnoreFile) ->
            {
                Path = above.Location.FullName
                WouldIgnore = IgnoreFile.isIgnoredFile env.Log (Some above) file
                Matches = IgnoreFile.matchingLines above file
            }
        )

    // The verdict comes from the same function every run uses, and the lines are the same
    // question asked one pattern at a time. The verdict is the one to report: it is what
    // decides what happens to the file, and a disagreement between the two is this command's
    // to survive rather than to resolve.
    IgnoreStep.Governed(
        ignoreFile.Location.FullName,
        IgnoreFile.isIgnoredFile env.Log (Some ignoreFile) file,
        IgnoreFile.matchingLines ignoreFile file,
        shadowed
    )

// The comment search as well as the comparison: the search cares about the order of the comments
// and the comparison does not, and a format run refuses what the search finds, which is what this
// command is reached for to explain.
let doctorValidations: Validations =
    Validations.CommentSearch
    ||| Validations.Parse
    ||| Validations.TriviaComparison
    ||| Validations.Idempotency

let formatOnce (isSignature: bool) (config: FormatConfig) (content: string) : Fantomas.Core.FormatResult =
    CodeFormatter.FormatDocumentWithValidationsAsync(isSignature, content, config, validations = doctorValidations)
    |> Async.RunSynchronously

let walkFormatting
    (report: DoctorReport)
    (format: string -> Fantomas.Core.FormatResult)
    (content: string)
    : DoctorReport
    =
    let original: string array = lines content

    let result: Result<Fantomas.Core.FormatResult, exn> =
        try
            Ok(format content)
        with error ->
            Error error

    match result with
    | Error error ->
        { report with
            Format = Some(FormatStep.Failed error)
        }
    | Ok result ->

    let formatted: string = result.Code
    let after: string array = lines formatted

    // Whether the file would be rewritten is asked of the text as it is, which is what a format run
    // compares and therefore what this has to predict. `lines` folds the line endings away, so a
    // file whose endings are the only thing the configuration disagrees with parts at no line at
    // all and would otherwise be reported as already formatted, while a run over it rewrites every
    // line of it.
    let change: FormatChange =
        match firstDifference original after with
        | Some line -> FormatChange.Reformatted(line, after.Length)
        | None ->

        if content <> formatted then
            FormatChange.LineEndingsOnly
        else
            FormatChange.Nothing

    let report: DoctorReport =
        { report with
            Format = Some(FormatStep.Produced(formatted, change))
        }

    match
        result.Issues
        |> List.filter (fun (issue: ValidationIssue) -> issue.IsNotValidFSharp)
    with
    | _ :: _ as invalid ->
        // The comments the search did not find are what a format run refuses the result over too, so
        // the Comments step says them. The comparison is left out, as it only reads the combinations
        // the output parses under, and formatting does not run a second pass over output with errors.
        let notFound: ValidationIssue list =
            result.Issues
            |> List.filter (fun (issue: ValidationIssue) -> issue.IsMissingComment)

        { report with
            Validity = Some(ValidityStep.Invalid invalid)
            Trivia =
                Some(
                    match notFound with
                    | [] -> TriviaStep.Kept
                    | notFound -> TriviaStep.Changed notFound
                )
        }
    | [] ->

    // Not a reason to stop the walk: the result is valid F#, so a second pass over it still has
    // something to say.
    let trivia: TriviaStep =
        match
            result.Issues
            |> List.filter (fun (issue: ValidationIssue) ->
                match issue with
                | ValidationIssue.MissingComment _
                | ValidationIssue.CommentsChanged _
                | ValidationIssue.DirectivesChanged _
                | ValidationIssue.CheckFailed(Validations.TriviaComparison, _) -> true
                | _ -> false
            )
        with
        | [] -> TriviaStep.Kept
        | changes -> TriviaStep.Changed changes

    let idempotency: IdempotencyStep =
        result.Issues
        |> List.tryPick (fun (issue: ValidationIssue) ->
            match issue with
            | ValidationIssue.CheckFailed(Validations.Idempotency, error) -> Some(IdempotencyStep.Failed error)
            | ValidationIssue.NotIdempotent again ->
                // Where the two passes part. When nothing but their line endings differs, which
                // `lines` folds away, they part at the start.
                let line: int =
                    firstDifference (lines formatted) (lines again) |> Option.defaultValue 1

                Some(IdempotencyStep.NotIdempotent(line, formatted, again))
            | _ -> None
        )
        |> Option.defaultValue IdempotencyStep.Idempotent

    { report with
        Validity = Some ValidityStep.Valid
        Trivia = Some trivia
        Idempotency = Some idempotency
    }

let diagnose (env: CliEnvironment) (given: string) : DoctorReport =
    let fs: IFileSystem = env.FileSystem
    let path: string = fs.Path.GetFullPath given
    let content: string = fs.File.ReadAllText path
    let file: DoctorFile = describeFile fs path content

    let report: DoctorReport =
        { stoppedAt (FileStep.Candidate file) with
            Ignore = Some(askIgnore env path)
        }

    match report.Ignore with
    // Nothing below this happens to the file, so nothing below this is reported about it. A run
    // that formatted it anyway would answer a question nobody asked, in a report whose whole
    // subject is why the file was left alone.
    | Some(IgnoreStep.Governed(_, true, _, _)) -> report
    | _ ->

    let settings: ResolvedConfig = env.ResolveConfiguration path

    let report: DoctorReport = { report with Settings = Some settings }

    let isSignature: bool = file.Kind = FileKind.Signature

    // Parsed on its own first, as formatting would: a file that is not valid F# is the file's
    // problem rather than Fantomas', and one that is shows the define combinations every step after
    // this one is read under.
    let parsed: Result<string list list, exn> =
        try
            CodeFormatter.ParseAsync(isSignature, content)
            |> Async.RunSynchronously
            |> Array.toList
            |> List.map snd
            |> Ok
        with error ->
            Error error

    match parsed with
    | Error error ->
        { report with
            Parse = Some(ParseStep.Failed error)
        }
    | Ok combinations ->

    walkFormatting
        { report with
            Parse = Some(ParseStep.Parsed combinations)
        }
        (formatOnce isSignature settings.Config)
        content

let runDoctorCommand (env: CliEnvironment) (inputPath: InputPath) : DoctorCommandResult =
    try
        match inputPath with
        | InputPath.Folder _
        | InputPath.Multiple _ -> DoctorCommandResult.NotOneFile inputPath
        // Absolute, as every path this command reports is, and `GetFullPath` answers for a path
        // that is not there as readily as for one that is. Resolving it against the folder the run
        // was started in is what turns "not found" into "not found here", which is the answer.
        | InputPath.NotFound path ->
            DoctorCommandResult.Completed(stoppedAt (FileStep.NotFound(env.FileSystem.Path.GetFullPath path)))
        | InputPath.NoFSharpFile path ->
            DoctorCommandResult.Completed(stoppedAt (FileStep.NotFSharp(env.FileSystem.Path.GetFullPath path)))
        | InputPath.File path -> DoctorCommandResult.Completed(diagnose env path)
    with error ->
        DoctorCommandResult.Failed error
