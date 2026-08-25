module Fantomas.Report

open System
open System.IO.Abstractions
open Serilog
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.ProfileCommand
open Fantomas.Logging
open Fantomas.Theme

// The context lines a parse failure's snippet is drawn from come from the file itself. This is
// the error path's last act before the tool gives up on the file, so reading it again is free.
let sourceOf (fs: IFileSystem) (file: string) : string =
    try
        fs.File.ReadAllText file
    with _ ->
        String.Empty

/// The results of a run, sorted into the states a file can end in, each in the order the files were
/// given.
///
/// A record rather than a tuple because the two `(string * ProfileInfo option) list` fields are the
/// same type as each other, so position was the only thing telling them apart.
[<NoComparison; NoEquality>]
type Outcomes =
    {
        Formatted: string list
        Unchanged: string list
        Ignored: string list
        Errored: (string * exn) list
    }

    /// Every file that was read, which is what the summary counts and what tells a run that looked
    /// at nothing apart from one that found nothing to do.
    member this.Count: int =
        List.length this.Formatted
        + List.length this.Unchanged
        + List.length this.Ignored
        + List.length this.Errored

let outcomes (results: FormatResult array) : Outcomes =
    // One pass, appending as it goes, so each list comes out in the order the files were given and
    // there is nothing to reverse afterwards. Every case is named once, and named here rather than
    // swept up by a wildcard, so a new one has to be placed deliberately.
    let formatted: ResizeArray<string> = ResizeArray()
    let unchanged: ResizeArray<string> = ResizeArray()
    let ignored: ResizeArray<string> = ResizeArray()
    let errored: ResizeArray<string * exn> = ResizeArray()

    for result in results do
        match result with
        | FormatResult.Formatted(file, _) -> formatted.Add file
        | FormatResult.Unchanged file -> unchanged.Add file
        | FormatResult.IgnoredFile file -> ignored.Add file
        | FormatResult.Error(file, error) -> errored.Add(file, error)
        // Formatting produced something that is not F#, which is a failure of Fantomas rather than
        // of the file it was given, and it is reported as one.
        | FormatResult.InvalidCode(file, _) -> errored.Add(file, invalidResultException file :> exn)

    {
        Formatted = List.ofSeq formatted
        Unchanged = List.ofSeq unchanged
        Ignored = List.ofSeq ignored
        Errored = List.ofSeq errored
    }

// A DefineParseException is a FormatException, so it has to be matched first or its own wording
// is never reached.
let describeFailure (error: exn) : string option =
    match error with
    | :? DefineParseException as dpe ->
        let combinations: string =
            dpe.Combinations
            |> List.map (fun c -> if c = "no defines" then "no defines" else $"[%s{c}]")
            |> String.concat ", "

        Some
            $"When Fantomas encounters #if directives in a file, it tries to format all possible combinations of defines and will merge all different versions back into one.\nFor %s{combinations}, however, we were not able to parse the file.\nWhile you may not use this combination in your project, Fantomas requires it to produce valid code.\nConsider fixing the code or ignoring this file.\nFor more information see: https://fsprojects.github.io/fantomas/docs/end-users/ConditionalCompilationDirectives.html"
    | :? FormatException as fe -> Some fe.Message
    | _ -> None

// One file's line: the status glyph, then the path, then what happened to it. Every state is
// written this way, so the path sits in the same column and the column can be read down.
let fileLine (theme: Theme) (glyph: string) (file: string) (tail: string) : string =
    String.Concat(glyph, " ", link theme file, " ", tail)

// A label that reads for one file as well as for many. The participles do not need this, since
// "1 formatted" is as good as "2 formatted", but a verb and a noun both do.
let plural (count: int) (singular: string) (many: string) : string = if count = 1 then singular else many

let summaryLine (theme: Theme) (counts: (int * string) list) : string =
    counts
    |> List.choose (fun (count: int, label: string) ->
        if count > 0 then
            Some(String.Concat(heading theme (string<int> count), " ", label))
        else
            None
    )
    |> String.concat ", "
    |> fun parts -> String.Concat(parts, ".")

let reportError (env: CliEnvironment) (verbosity: VerbosityLevel) (file: string, error: exn) : unit =
    let glyphs: StatusGlyphs = statusGlyphs env.ErrorTheme

    let describeOther () : string =
        let message: string =
            match verbosity with
            | VerbosityLevel.Detailed -> $"%A{error}"
            | VerbosityLevel.Normal -> describeFailure error |> Option.defaultValue String.Empty

        if String.IsNullOrEmpty message then
            String.Concat(glyphs.Errored, " ", link env.ErrorTheme file, " could not be formatted.")
        else
            String.Concat(glyphs.Errored, " ", link env.ErrorTheme file, " could not be formatted: ", message)

    let source () : string = sourceOf env.FileSystem file
    let verbose: bool = verbosity = VerbosityLevel.Detailed

    // A parse failure and an invariant violation both describe themselves, positions and all,
    // rather than being reduced to a single line saying only that they happened. The first line of
    // each is its header, so the glyph goes in front of the whole block and lands in the column
    // every other state puts it in.
    match Diagnostics.describeParseFailure file source error with
    | Some report -> env.Log.Error(String.Concat(glyphs.Errored, " ", report))
    | None ->

    match Diagnostics.describeInvariantViolation file source verbose error with
    | Some report -> env.Log.Error(String.Concat(glyphs.Errored, " ", report))
    | None -> env.Log.Error(describeOther ())

// A single named file is answered on its own terms: the caller asked about this file, so every
// state it can be in is said out loud, and there is no summary to add to one line.
let reportSingleResult (env: CliEnvironment) (settings: CliSettings) (result: FormatResult) : unit =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme

    match result with
    | FormatResult.IgnoredFile f ->
        env.Log.Information(fileLine theme glyphs.Ignored f "was ignored by .fantomasignore.")
    | FormatResult.Error(f, e) -> reportError env settings.Verbosity (f, e)
    | FormatResult.Formatted(f, _) -> env.Log.Information(fileLine theme glyphs.Formatted f "was formatted.")
    | FormatResult.Unchanged f -> env.Log.Information(fileLine theme glyphs.Unchanged f "was unchanged.")
    | FormatResult.InvalidCode(f, _) ->
        let ex: FormatException = invalidResultException f
        reportError env settings.Verbosity (f, ex)

let reportFormatResults
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    (results: FormatResult array)
    : unit
    =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme
    let paths: string = describeInputPaths inputPath

    let outcome: Outcomes = outcomes results

    // A single result is a single named file, and it is answered as itself whatever state it is
    // in. The two warnings below are about a scan that came to nothing, which is a different
    // thing from a file the caller pointed at.
    match Array.tryExactlyOne results with
    | Some single -> reportSingleResult env settings single
    | None when outcome.Count = 0 ->
        // Nothing was looked at, which a silent exit 0 would have read as success. This is the run
        // a bad glob or an over broad ignore file produces, and it has to say so.
        env.Log.Warning $"No F# files found in %s{paths}."
    | None when outcome.Count = List.length outcome.Ignored ->
        env.Log.Warning $"All %d{List.length outcome.Ignored} F# files in %s{paths} were ignored by .fantomasignore."
    | None ->
        // Only what changed is listed. Everything else is a count, which is what keeps a run over a
        // formatted tree to one line.
        for file in outcome.Formatted do
            env.Log.Information(fileLine theme glyphs.Formatted file "was formatted.")

        for e in outcome.Errored do
            reportError env settings.Verbosity e

        // A scan that skipped files stays quiet about them at normal verbosity: the count in the
        // summary is the whole story unless someone asks for more.
        for file in outcome.Ignored do
            env.Log.Debug $"'%s{file}' was ignored"

        let summary: string =
            match outputPath with
            // Under --out nothing is really unchanged: every input produces an output file, so the
            // count that means something is how many were written.
            | OutputPath.IO destination ->
                let written: int = List.length outcome.Formatted + List.length outcome.Unchanged

                let noun: string = plural written "file" "files"

                summaryLine
                    theme
                    [
                        written, $"%s{noun} written to %s{destination}"
                        List.length outcome.Formatted, "reformatted"
                    ]
            | OutputPath.NotKnown ->
                summaryLine
                    theme
                    [
                        List.length outcome.Formatted, "formatted"
                        List.length outcome.Unchanged, "unchanged"
                        List.length outcome.Ignored, "ignored"
                        List.length outcome.Errored, "errored"
                    ]

        // A separator only where there is something to separate from. A run over an already
        // formatted tree is one line and does not want a blank one above it.
        if not (List.isEmpty outcome.Formatted) then
            env.Log.Information ""

        env.Log.Information summary

let describeInputProblem (problem: InputProblem) : string =
    match problem with
    | InputProblem.UnsupportedFileType path -> $"Input path '%s{path}' is an unsupported file type."
    | InputProblem.NotFound path -> $"Input path '%s{path}' not found."
    | InputProblem.MultiplePathsWithOut -> "Multiple input files are not supported with the --out flag."

// The gap between the three columns of the profile table. The columns themselves are as wide as
// what goes in them, measured before anything is written: a fixed width truncates or overflows the
// moment a path is longer than someone guessed, and the paths here are whatever the caller's tree
// is called.
let profileGap: int = 4

let describeMilliseconds (span: TimeSpan) : string =
    // Milliseconds, because a formatter measured in `mm:ss.fff` spends its width on zeros and
    // stops making sense after an hour.
    $"%.0f{span.TotalMilliseconds}ms"

// The three columns, already padded and coloured, joined into a line.
//
// Padding happens on the plain text and the colour goes on after, which is what lets `PadLeft` and
// `PadRight` do it: they count characters, and an escape sequence is characters that take no width
// on screen. Colouring first and measuring after is what `Theme.visibleLength` exists for, and is
// the harder way round when the caller has the plain text in hand anyway.
let profileRow (file: string) (lines: string) (time: string) (note: string) : string =
    String.Concat("  ", file, String(' ', profileGap), lines, String(' ', profileGap), time, note)

// Said only where it is not one, which is nearly everywhere, so the line stays quiet unless there
// is something about it to explain.
let describeCombinations (theme: Theme) (combinations: int) : string =
    if combinations <= 1 then
        ""
    else
        muted theme $"  (%d{combinations} define combinations)"

let reportProfileResult (env: CliEnvironment) (result: ProfileResult) : unit =
    let theme: Theme = env.OutputTheme

    let total: int =
        List.sumBy (fun (timing: FileTiming) -> timing.LineCount) result.Timings

    let totalLabel: string = "Total"
    let totalLines: string = $"%d{total} lines"
    let totalTime: string = describeMilliseconds result.Elapsed

    // Measured before anything is written, from what actually goes in each column. A fixed width
    // truncates or overflows the moment a path is longer than someone guessed, and the paths here
    // are whatever the caller's tree is called.
    let widthOf (pick: FileTiming -> string) (ofTotal: string) : int =
        result.Timings
        |> List.map (fun timing -> String.length (pick timing))
        |> List.fold max (String.length ofTotal)

    let fileWidth: int = widthOf (fun timing -> timing.File) totalLabel

    let linesWidth: int =
        widthOf (fun timing -> $"%d{timing.LineCount} lines") totalLines

    let timeWidth: int =
        widthOf (fun timing -> describeMilliseconds timing.TimeTaken) totalTime

    env.Log.Information
        $"Formatted %d{List.length result.Timings} files serially so the timings are comparable. Nothing was written."

    env.Log.Information ""

    for timing in result.Timings do
        env.Log.Information(
            profileRow
                (link theme (timing.File.PadRight fileWidth))
                (placeholder theme ($"%d{timing.LineCount} lines".PadLeft linesWidth))
                (describeMilliseconds(timing.TimeTaken).PadLeft timeWidth)
                (describeCombinations theme timing.DefineCombinations)
        )

    env.Log.Information ""

    env.Log.Information(
        profileRow
            (heading theme (totalLabel.PadRight fileWidth))
            (placeholder theme (totalLines.PadLeft linesWidth))
            (heading theme (totalTime.PadLeft timeWidth))
            ""
    )

    for file in result.Ignored do
        env.Log.Debug $"'%s{file}' was ignored"

let reportProfileCommand
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputPath: InputPath)
    (result: ProfileCommandResult)
    : int
    =
    match result with
    | ProfileCommandResult.InvalidInput problem -> env.Log.Error(describeInputProblem problem)
    | ProfileCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | ProfileCommandResult.Completed profile ->
        if List.isEmpty profile.Timings && List.isEmpty profile.Errors then
            env.Log.Warning $"No F# files found in %s{describeInputPaths inputPath}."
        else
            reportProfileResult env profile

        for failure in profile.Errors do
            reportError env settings.Verbosity failure

    result.ExitCode

let reportCheckResults (env: CliEnvironment) (inputPath: InputPath) (checkResult: CheckResult) : unit =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme
    let errorGlyphs: StatusGlyphs = statusGlyphs env.ErrorTheme

    for filename, exn in checkResult.Errors do
        let source () : string = sourceOf env.FileSystem filename

        match Diagnostics.describeParseFailure filename source exn with
        | Some report -> env.Log.Error(String.Concat(errorGlyphs.Errored, " ", report))
        | None ->

        match Diagnostics.describeInvariantViolation filename source false exn with
        | Some report -> env.Log.Error(String.Concat(errorGlyphs.Errored, " ", report))
        | None ->
            // The message rather than `exn.ToString()`, which carried a stack trace through
            // build agent paths into what a user reads.
            env.Log.Error(
                String.Concat(
                    errorGlyphs.Errored,
                    " ",
                    link env.ErrorTheme filename,
                    " could not be checked: ",
                    exn.Message
                )
            )

    for filename in checkResult.Formatted do
        env.Log.Information(fileLine theme glyphs.NeedsFormatting filename "needs formatting.")

    let needing: int = List.length checkResult.Formatted

    if needing > 0 then
        // The command that fixes it, spelled the way this run was started, so it is a line the
        // caller can run again rather than one they have to translate.
        let fix: string =
            let subject: string = if needing = 1 then "it" else "them"

            String.Concat(
                "Run ",
                muted theme (Invocation.name ()),
                flagName theme (String.Concat(" ", describeInputPaths inputPath)),
                $" to format %s{subject}."
            )

        let summary: string =
            summaryLine
                theme
                [
                    needing, plural needing "needs formatting" "need formatting"
                    List.length checkResult.Unchanged, "already formatted"
                    List.length checkResult.Errors, "errored"
                ]

        env.Log.Information ""
        env.Log.Information(String.Concat(summary, " ", fix))

// What is printed and what the process ends with are decided apart from each other, so that this
// reporter and the JSON one cannot end the same run with different codes.
let reportFormatCommand
    (env: CliEnvironment)
    (settings: CliSettings)
    (inputPath: InputPath)
    (outputPath: OutputPath)
    (result: FormatCommandResult)
    : int
    =
    match result with
    | FormatCommandResult.InvalidInput problem -> env.Log.Error(describeInputProblem problem)
    | FormatCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | FormatCommandResult.Completed results -> reportFormatResults env settings inputPath outputPath results

    result.ExitCode

let reportCheckCommand (env: CliEnvironment) (inputPath: InputPath) (result: CheckCommandResult) : int =
    match result with
    | CheckCommandResult.InvalidInput problem -> env.Log.Error(describeInputProblem problem)
    | CheckCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | CheckCommandResult.Completed(ignored, checkResult) ->
        // A file the caller named explicitly and that was skipped is worth saying out loud: a
        // silent exit 0 reads as "already formatted" when nothing was looked at at all.
        match ignored, checkResult.Errors, checkResult.Formatted, checkResult.Unchanged with
        | [ file ], [], [], [] ->
            let glyphs: StatusGlyphs = statusGlyphs env.OutputTheme
            env.Log.Information(fileLine env.OutputTheme glyphs.Ignored file "was ignored by .fantomasignore.")
        | _ ->
            for file in ignored do
                env.Log.Debug $"'%s{file}' was ignored"

            reportCheckResults env inputPath checkResult

    result.ExitCode
