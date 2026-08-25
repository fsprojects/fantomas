module Fantomas.Report

open System
open System.IO.Abstractions
open System.Text
open Serilog
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.CommandResult
open Fantomas.ProfileCommand
open Fantomas.DoctorCommand
open Fantomas.EditorConfig
open Fantomas.Logging
open Fantomas.Theme

// The context lines a parse failure's snippet is drawn from come from the file itself. This is
// the error path's last act before the tool gives up on the file, so reading it again is free.
let sourceOf (fs: IFileSystem) (file: string) : string =
    try
        fs.File.ReadAllText file
    with _ ->
        String.Empty

/// The results of a run, sorted into the states a file can end in, each by path.
///
/// By path rather than in the order the work came back, which for a folder is the order the file
/// system happened to hand the directory over in. That order is neither the one the caller gave nor
/// one worth promising, and it makes two runs of the same command on two machines print the same
/// files in different orders. The JSON report already sorts, for the same reason and in the same
/// words.
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
    // One pass, appending as it goes. Every case is named once, and named here rather than swept up
    // by a wildcard, so a new one has to be placed deliberately.
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
        // Formatting produced output Fantomas would not accept, which is a failure of Fantomas
        // rather than of the file it was given, and it is reported as one.
        | FormatResult.InvalidCode(file, formattedContent, diagnostics) ->
            errored.Add(file, InvalidCodeException(formattedContent, diagnostics) :> exn)

    {
        Formatted = List.ofSeq formatted |> List.sort
        Unchanged = List.ofSeq unchanged |> List.sort
        Ignored = List.ofSeq ignored |> List.sort
        Errored = List.ofSeq errored |> List.sortBy fst
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

/// The paths the caller typed, as far as they name files.
///
/// Every path on the command line is accounted for somewhere: a folder by the counts it produces, a
/// file by a count it is part of or by a line of its own. Skipped is the one state no count can
/// carry, because a count cannot see a folder that was never opened, so for a file somebody typed a
/// line is the only place left. For a file the walk turned up inside a folder there is no such
/// obligation, and listing them would put a vendored checkout on the screen.
let namedOnTheCommandLine (inputPath: InputPath) : Set<string> =
    match inputPath with
    | InputPath.File file -> Set.singleton file
    | InputPath.Multiple(files, _) -> Set.ofList files
    | InputPath.Folder _
    | InputPath.NoFSharpFile _
    | InputPath.NotFound _ -> Set.empty

/// Say a skipped file out loud when the caller named it, and only to whoever asked for detail when
/// the walk turned it up. Both in one place, so that a file cannot get the sentence and the debug
/// line at once, which is how the same event came to have two spellings before.
///
/// Answers whether anything was said out loud, since that is what decides whether the summary has
/// something above it to be separated from.
let reportIgnored
    (env: CliEnvironment)
    (theme: Theme)
    (glyphs: StatusGlyphs)
    (named: Set<string>)
    (ignored: string list)
    : bool
    =
    let mutable saidOutLoud: bool = false

    for file in List.sort ignored do
        if Set.contains file named then
            saidOutLoud <- true
            env.Log.Information(fileLine theme glyphs.Ignored file "was ignored by .fantomasignore.")
        else
            env.Log.Debug $"'%s{file}' was ignored"

    saidOutLoud

// A label that reads for one file as well as for many. The participles do not need this, since
// "1 formatted" is as good as "2 formatted", but a verb and a noun both do.
let plural (count: int) (singular: string) (many: string) : string = if count = 1 then singular else many

// The counts a run ended with, as one line, with what was counted said once at the head and carried
// across the rest: `2 files formatted, 30 unchanged`, not `2 files formatted, 30 files unchanged`,
// and not a bare `32 unchanged` with nothing in it to say what there were thirty two of. The noun
// agrees with the count it is attached to rather than with the total, since it belongs to that
// first phrase and not to the sentence.
let summaryLine (theme: Theme) (counts: (int * string) list) : string =
    let line: StringBuilder = StringBuilder()

    for count: int, label: string in counts do
        if count > 0 then
            // Nothing written yet means this is the first count that survived, and the first is the
            // one that carries the noun. Asking the line what it has so far is what lets the states
            // at zero be dropped, the separator be placed and the noun be put in front of exactly
            // one of them without walking the list three times to find out.
            if line.Length = 0 then
                line.Append(heading theme (string<int> count)).Append(' ').Append(plural count "file" "files")
                |> ignore
            else
                line.Append(", ").Append(heading theme (string<int> count)) |> ignore

            line.Append(' ').Append(label) |> ignore

    line.Append('.').ToString()

/// Render `error` as the report it writes for itself, when it is one of the failures that has more
/// to say than a single line stating that it happened. A parse failure and an invariant violation
/// both draw a caret under the thing they are about; output Fantomas would not accept has no
/// position to give but still has more to say than one line holds. Anything else comes back as
/// `None`, for the caller to reduce to a line of its own wording.
///
/// Asked in one place because both commands ask it. They had a copy of this each, and the copies
/// drifted the moment a third failure was given a report of its own: a check run printed the whole
/// explanation after `could not be checked:` while a format run printed the block.
///
/// `source` yields the text the failure came from and is called only by the two that draw a caret,
/// so a caller that has to read a file to produce it does not read one for a failure with nothing
/// to point at.
let describeItself (theme: Theme) (file: string) (source: unit -> string) (verbose: bool) (error: exn) : string option =
    match Diagnostics.describeParseFailure theme file source error with
    | Some report -> Some report
    | None ->

    match Diagnostics.describeInvariantViolation theme file source verbose error with
    | Some report -> Some report
    | None ->

    match error with
    | :? InvalidCodeException as invalid ->
        Some(Diagnostics.renderInvalidOutput theme file invalid.FormattedContent invalid.Diagnostics)
    | _ -> None

let reportError (env: CliEnvironment) (verbosity: VerbosityLevel) (file: string, error: exn) : unit =
    let glyphs: StatusGlyphs = statusGlyphs env.ErrorTheme

    // Whatever the failure has to say for itself, at any verbosity. It used to be dropped unless
    // detailed verbosity was asked for, which meant an unreadable file reported only that it could
    // not be formatted, and the one sentence that explained it, `Access to the path is denied`, was
    // behind a flag nobody knew to pass. Fantomas has its own wording for the failures it knows;
    // everything else says the exception's message rather than nothing.
    let describeOther () : string =
        let message: string = describeFailure error |> Option.defaultValue error.Message

        if String.IsNullOrEmpty message then
            String.Concat(glyphs.Errored, " ", link env.ErrorTheme file, " could not be formatted.")
        else
            String.Concat(glyphs.Errored, " ", link env.ErrorTheme file, " could not be formatted: ", message)

    let source () : string = sourceOf env.FileSystem file
    let verbose: bool = verbosity = VerbosityLevel.Detailed

    // The first line of a report that describes itself is its header, so the glyph goes in front of
    // the whole block and lands in the column every other state puts it in.
    match describeItself env.ErrorTheme file source verbose error with
    | Some report -> env.Log.Error(String.Concat(glyphs.Errored, " ", report))
    | None ->
        env.Log.Error(describeOther ())

        // The line above is the one to act on; this keeps the type and the stack trace for whoever
        // asks for detail, rather than replacing the message with them.
        env.Log.Debug $"%A{error}"

// A single named file is answered on its own terms: the caller asked about this file, so every
// state it can be in is said out loud, and there is no summary to add to one line.
//
// Which means the one line has to carry what the summary would have said. Under `--out` a file
// appears at the destination whether or not its content changed, and saying only `was unchanged`
// left the run's whole effect unmentioned: a file was written to a folder the line never named.
// So the two states that write name where they wrote, and the two that do not are left alone.
let reportSingleResult
    (env: CliEnvironment)
    (settings: CliSettings)
    (outputPath: OutputPath)
    (result: FormatResult)
    : unit
    =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme

    let formatted: string =
        match outputPath with
        | OutputPath.NotKnown -> "was formatted."
        | OutputPath.IO destination -> $"was formatted and written to %s{destination}."

    let unchanged: string =
        match outputPath with
        | OutputPath.NotKnown -> "was unchanged."
        | OutputPath.IO destination -> $"was written to %s{destination} unchanged."

    match result with
    | FormatResult.IgnoredFile f ->
        env.Log.Information(fileLine theme glyphs.Ignored f "was ignored by .fantomasignore.")
    | FormatResult.Error(f, e) -> reportError env settings.Verbosity (f, e)
    | FormatResult.Formatted(f, _) -> env.Log.Information(fileLine theme glyphs.Formatted f formatted)
    | FormatResult.Unchanged f -> env.Log.Information(fileLine theme glyphs.Unchanged f unchanged)
    | FormatResult.InvalidCode(f, formattedContent, diagnostics) ->
        let ex: InvalidCodeException = InvalidCodeException(formattedContent, diagnostics)
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
    | Some single -> reportSingleResult env settings outputPath single
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

        let saidIgnored: bool =
            reportIgnored env theme glyphs (namedOnTheCommandLine inputPath) outcome.Ignored

        // Unchanged is carried by the count, so a folder run says no more about it than that. Said
        // here rather than where each file is formatted, because here is what knows whether the
        // state has already been said out loud: `formatSource` logged it itself, and a run over one
        // file printed both that and the sentence below it, one event in two spellings.
        for file in outcome.Unchanged do
            env.Log.Debug $"'%s{file}' was unchanged"

        let summary: string =
            match outputPath with
            // Under --out nothing is really unchanged: every input produces an output file, so the
            // count that means something is how many were written.
            | OutputPath.IO destination ->
                let written: int = List.length outcome.Formatted + List.length outcome.Unchanged

                // Errored is counted here as well. Leaving it out left a run where every file
                // failed with `.` as its whole summary.
                summaryLine
                    theme
                    [
                        written, $"written to %s{destination}"
                        List.length outcome.Formatted, "reformatted"
                        List.length outcome.Errored, "errored"
                    ]
            | OutputPath.NotKnown ->
                summaryLine
                    theme
                    [
                        List.length outcome.Formatted, "formatted"
                        List.length outcome.Unchanged, "unchanged"
                        List.length outcome.Errored, "errored"
                    ]

        // A separator only where there is something to separate from. A run over an already
        // formatted tree is one line and does not want a blank one above it.
        if not (List.isEmpty outcome.Formatted) || saidIgnored then
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

// "1 line" and "2 lines", said the same way wherever the count appears, so that the column measured
// against it is measured against what goes in it.
let describeLines (count: int) : string =
    String.Concat(string<int> count, " ", plural count "line" "lines")

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
    let totalLines: string = describeLines total
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
        widthOf (fun timing -> describeLines timing.LineCount) totalLines

    let timeWidth: int =
        widthOf (fun timing -> describeMilliseconds timing.TimeTaken) totalTime

    let timed: int = List.length result.Timings
    let files: string = plural timed "file" "files"

    env.Log.Information $"Formatted %d{timed} %s{files} serially so the timings are comparable. Nothing was written."

    env.Log.Information ""

    for timing in result.Timings do
        env.Log.Information(
            profileRow
                (link theme (timing.File.PadRight fileWidth))
                (placeholder theme ((describeLines timing.LineCount).PadLeft linesWidth))
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

let reportCheckResults
    (env: CliEnvironment)
    (inputPath: InputPath)
    (ignored: string list)
    (checkResult: CheckResult)
    : unit
    =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme
    let errorGlyphs: StatusGlyphs = statusGlyphs env.ErrorTheme

    for filename, exn in List.sortBy fst checkResult.Errors do
        let source () : string = sourceOf env.FileSystem filename

        match describeItself env.ErrorTheme filename source false exn with
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

    for filename in List.sort checkResult.Formatted do
        env.Log.Information(fileLine theme glyphs.NeedsFormatting filename "needs formatting.")

    reportIgnored env theme glyphs (namedOnTheCommandLine inputPath) ignored
    |> ignore

    let needing: int = List.length checkResult.Formatted
    let errored: int = List.length checkResult.Errors

    // Every file the run looked at, skipped ones included, so that this and the format command
    // agree on what counts as a run over one file.
    let looked: int =
        needing + errored + List.length checkResult.Unchanged + List.length ignored

    // The command that fixes it, spelled the way this run was started, so it is a line the caller
    // can run again rather than one they have to translate. Only where there is something to fix:
    // a run whose only finding is a file that would not parse has no formatting to suggest.
    let fix: string option =
        if needing = 0 then
            None
        else
            let subject: string = if needing = 1 then "it" else "them"

            Some(
                String.Concat(
                    "Run ",
                    muted theme env.Invocation,
                    flagName theme (String.Concat(" ", describeInputPaths inputPath)),
                    $" to format %s{subject}."
                )
            )

    // Counted whenever the run found anything, rather than only when something needs formatting: an
    // error is a finding too, and a check that reported one used to end without saying how many
    // files it had looked at to find it.
    //
    // What an ignore file skipped is not among the counts, here or in a format run. A pattern that
    // names a file can be counted and a pattern that names a folder cannot, because the folder is
    // never opened, and a number that is right about the first and blind to the second reads as
    // though it covered both: this repository skips ninety six files through three folder patterns
    // and the count said nought. Both are named at detailed verbosity, where each is exact.
    //
    // Left out for a single file, the way a format run leaves it out: the line above already named
    // the file and said what was found, and `1 needs formatting` only says it again.
    let summary: string option =
        if looked <= 1 || (needing = 0 && errored = 0) then
            None
        else
            Some(
                summaryLine
                    theme
                    [
                        needing, plural needing "needs formatting" "need formatting"
                        List.length checkResult.Unchanged, "already formatted"
                        errored, "errored"
                    ]
            )

    match List.choose id [ summary; fix ] with
    | [] -> ()
    | lines ->
        env.Log.Information ""
        env.Log.Information(String.concat " " lines)

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
        let paths: string = describeInputPaths inputPath

        let looked: int =
            List.length ignored
            + List.length checkResult.Errors
            + List.length checkResult.Formatted
            + List.length checkResult.Unchanged

        // A file the caller named explicitly and that was skipped is worth saying out loud: a
        // silent exit 0 reads as "already formatted" when nothing was looked at at all.
        match ignored, checkResult.Errors, checkResult.Formatted, checkResult.Unchanged with
        | [ file ], [], [], [] ->
            let glyphs: StatusGlyphs = statusGlyphs env.OutputTheme
            env.Log.Information(fileLine env.OutputTheme glyphs.Ignored file "was ignored by .fantomasignore.")
        | _ ->
            // The two runs that come to nothing, which a check used to pass over in silence and a
            // format run has always spoken up about. Silence is how this command says every file is
            // already formatted, so a run that looked at no file at all cannot also be silent: a bad
            // glob or an ignore file that grew too wide would read as a green build forever.
            //
            // A warning, so it lands on standard error and leaves standard out to the findings, and
            // the exit code stays 0 because nothing was found to be wrong.
            if looked = 0 then
                env.Log.Warning $"No F# files found in %s{paths}."
            elif looked = List.length ignored then
                env.Log.Warning $"All %d{looked} F# files in %s{paths} were ignored by .fantomasignore."
            else
                reportCheckResults env inputPath ignored checkResult

    result.ExitCode

// One row of the doctor report: the status column, the step's name, what that step came to, and
// the lines that belong under it.
[<NoComparison; NoEquality>]
type DoctorRow =
    {
        Glyph: string
        Label: string
        Says: string
        Detail: string list
    }

let doctorRow (glyph: string) (label: string) (says: string) : DoctorRow =
    {
        Glyph = glyph
        Label = label
        Says = says
        Detail = []
    }

// `a`, `a and b`, `a, b and c`. Only the steps that were not looked at and the `.editorconfig`
// files that set something are read out this way, and a list read out as `a; b; c` is a data
// structure rather than a sentence.
//
// Walked once, from the front, which is the order the words are already in: the last two are the
// pair that takes `and`, and every word before them takes a comma. Reversing to find the last one
// and reversing back to put the rest in order was two passes to learn something the shape of the
// match says on its own.
let rec andList (words: string list) : string =
    match words with
    | [] -> String.Empty
    | [ only ] -> only
    | [ nextToLast; last ] -> String.Concat(nextToLast, " and ", last)
    | first :: rest -> String.Concat(first, ", ", andList rest)

// The gap between the status column and what each step says, measured from the longest label
// rather than fixed, so adding a step cannot leave the column too narrow for its own name.
let doctorGap: int = 2

let doctorColumn (rows: DoctorRow list) : int =
    rows
    // The glyph is one character wide and the space after it is the second.
    |> List.map (fun (row: DoctorRow) -> 2 + String.length row.Label + doctorGap)
    |> List.fold max 0

let describeDoctorFile (theme: Theme) (glyphs: StatusGlyphs) (step: FileStep) : DoctorRow =
    match step with
    | FileStep.NotFound _ -> doctorRow glyphs.Errored "File" "There is no file at this path."
    | FileStep.NotFSharp _ ->
        doctorRow
            glyphs.Errored
            "File"
            "Found on disk, but not a file Fantomas formats. It formats .fs, .fsi, .fsx, .ml and .mli."
    | FileStep.Candidate file ->
        let kind: string =
            match file.Kind with
            | FileKind.Implementation -> "an implementation file"
            | FileKind.Signature -> "a signature file"
            | FileKind.Script -> "a script file"

        // Said out loud, because it is the first thing this command checked and the reader has to
        // be able to tell it apart from a step that was never run. A line that opens with what the
        // file is reads as a description of a file already taken for granted.
        let says: string = $"Found on disk: %s{kind} of %s{describeLines file.LineCount}."

        match file.UnreachableUnder with
        | None -> doctorRow glyphs.Formatted "File" says
        | Some folder ->
            // Not a failure: naming the file, which is what this run did, formats it. It is the
            // answer to the question that brings somebody here with a file under `obj`, and the
            // ignore file they were about to go and read has nothing to do with it.
            { doctorRow glyphs.NeedsFormatting "File" says with
                Detail =
                    [
                        $"It sits under %s{link theme folder}, which Fantomas never opens, so a run over a"
                        "folder above it does not reach this file. Naming the file itself, as here, does."
                    ]
            }

let describeIgnoreMatch (theme: Theme) (matched: IgnoreMatch) : string =
    String.Concat("line ", string<int> matched.LineNumber, ": ", flagName theme matched.Pattern)

let describeDoctorIgnore (theme: Theme) (glyphs: StatusGlyphs) (verbose: bool) (step: IgnoreStep) : DoctorRow =
    match step with
    | IgnoreStep.NoIgnoreFile -> doctorRow glyphs.Formatted "Ignore" "No .fantomasignore at or above this file."
    | IgnoreStep.Governed(ignoreFile, isIgnored, matches) ->
        let deciding: IgnoreMatch option = List.tryLast matches

        // A path is somewhere the reader can go and a pattern is something they can type, here as
        // in the lines below. A sentence is no reason for either to lose its colour.
        let ignoreFile: string = link theme ignoreFile
        let pattern (matched: IgnoreMatch) : string = flagName theme matched.Pattern

        let says: string =
            match isIgnored, deciding with
            | true, Some matched -> $"Matched by %s{ignoreFile}, line %d{matched.LineNumber}: %s{pattern matched}"
            | true, None ->
                // The ignore library said yes and no pattern of the file says so on its own. That
                // should not happen, and if it does the verdict is the one that decides what
                // happens to the file, so the verdict is what is reported.
                $"Matched by %s{ignoreFile}. Which pattern matched could not be worked out."
            | false, Some matched when matched.Negated ->
                $"Not matched: line %d{matched.LineNumber} of %s{ignoreFile}, %s{pattern matched}, takes it back out."
            | false, _ -> $"Governed by %s{ignoreFile}, and no pattern in it matches."

        // Read out one at a time only where there is more than one, since a single match is
        // already quoted in the line above. Several is where it earns its space: what decided is
        // the last of them, and a `!` line further down is exactly the case nobody spots by eye.
        let listed: string list =
            if List.length matches > 1 then
                List.map (describeIgnoreMatch theme) matches @ [ "The last of these decides." ]
            else
                []

        // The difference from `.gitignore` that catches people out, said to whoever asked for
        // detail rather than on every run.
        let nearestOnly: string list =
            if verbose then
                [
                    "This is the nearest .fantomasignore at or above the file and the only one that"
                    "applies. Unlike .gitignore, Fantomas does not merge in the ones above it."
                ]
            else
                []

        { doctorRow glyphs.Ignored "Ignore" says with
            Detail = listed @ nearestOnly
        }
        |> fun (row: DoctorRow) ->
            if isIgnored then
                row
            else
                { row with Glyph = glyphs.Formatted }

let describeDoctorSettings (theme: Theme) (glyphs: StatusGlyphs) (resolved: ResolvedConfig) : DoctorRow =
    let fromEditorConfig: ResolvedSetting list = resolved.FromEditorConfig
    let total: int = List.length resolved.Settings

    // The files that actually set something, rather than the whole chain that was read: an
    // `.editorconfig` the chain includes but Fantomas reads nothing out of did not contribute a
    // setting, and naming it as having would send somebody to edit the wrong file. Ordered as the
    // chain is, furthest away first, which is the order they are applied in.
    let contributing: string list =
        let origins: Set<string> =
            fromEditorConfig
            |> List.choose (fun (setting: ResolvedSetting) -> setting.SetBy)
            |> Set.ofList

        match
            resolved.EditorConfigFiles
            |> List.filter (fun file -> Set.contains file origins)
        with
        // The two lists come from the same parse, so this should not be reachable. Keep a way
        // through anyway, because that is the library's invariant and not ours, and name the whole
        // chain rather than name nothing.
        | [] -> resolved.EditorConfigFiles
        | contributing -> contributing

    // Read out as places the reader can go, the way the same paths are in the column below.
    let named (files: string list) : string =
        files |> List.map (link theme) |> andList

    let says: string =
        match resolved.EditorConfigFiles, fromEditorConfig with
        | [], _ -> $"No .editorconfig applies. All %d{total} settings are Fantomas defaults."
        | files, [] -> $"All %d{total} settings are Fantomas defaults: %s{named files} sets nothing Fantomas reads."
        | _, set ->
            let count: int = List.length set

            // Named, and named absolutely, as every path this report prints is. `.editorconfig` on
            // its own is the one thing somebody reading this cannot go and open.
            $"%d{count} of %d{total} settings come from %s{named contributing}, the rest are Fantomas defaults."

    let written (setting: ResolvedSetting) : string =
        String.Concat(setting.Setting, " = ", setting.Value)

    // Measured across every setting rather than per group, so the two groups line up as one table
    // and the origin column can be read straight down.
    let settingWidth: int =
        resolved.Settings |> List.map (written >> String.length) |> List.fold max 0

    let describeSetting (setting: ResolvedSetting) : string =
        let origin: string =
            match setting.SetBy with
            | Some file -> link theme file
            | None -> muted theme "the Fantomas default"

        String.Concat((written setting).PadRight(settingWidth + doctorGap), origin)

    // Every setting, in two groups with a blank line between them. What an `.editorconfig` decided
    // is what somebody came here to see, and it belongs at the top where they will see it; the rest
    // is what the file will actually be formatted with, which is the question the step is answering
    // and is not answered by a list with most of it left out.
    let settings: string list =
        let defaults: ResolvedSetting list =
            resolved.Settings
            |> List.filter (fun (setting: ResolvedSetting) -> setting.SetBy.IsNone)

        match fromEditorConfig, defaults with
        | [], only
        | only, [] -> List.map describeSetting only
        | set, defaults ->
            List.map describeSetting set
            @ [ String.Empty ]
            @ List.map describeSetting defaults

    // Set apart the same way, because they are sentences rather than rows of a table and forty rows
    // above them is exactly what a sentence gets lost under.
    let problems: string list =
        match resolved.Problems with
        | [] -> []
        | problems -> String.Empty :: List.map EditorConfigReport.describeProblem problems

    {
        Glyph =
            (if List.isEmpty resolved.Problems then
                 glyphs.Formatted
             else
                 glyphs.NeedsFormatting)
        Label = "Settings"
        Says = says
        Detail = settings @ problems
    }

let describeDoctorFormat (glyphs: StatusGlyphs) (lineCount: int) (step: FormatStep) : DoctorRow =
    match step with
    | FormatStep.Failed error ->
        let message: string = describeFailure error |> Option.defaultValue error.Message

        let says: string =
            if String.IsNullOrEmpty message then
                "Formatting failed."
            else
                // The whole of it is written below the table, where it has the width for a snippet
                // and a caret. This is the one line version, so the column can still be read down.
                String.Concat("Formatting failed: ", (message.Split('\n')).[0])

        doctorRow glyphs.Errored "Format" says
    | FormatStep.Produced(_, FormatChange.Nothing) ->
        doctorRow glyphs.Unchanged "Format" "Already formatted. Nothing would change."
    | FormatStep.Produced(_, FormatChange.LineEndingsOnly) ->
        // Worth its own sentence rather than a count of nought. It is the state that reads as
        // already formatted to everything that compares line by line, and the one a working tree
        // checked out with the other platform's endings is in.
        doctorRow
            glyphs.NeedsFormatting
            "Format"
            "Not formatted: every line is as it should be and the line endings are not, so the whole file would be rewritten."
    | FormatStep.Produced(_, FormatChange.Reformatted(firstChangedLine, lineCountAfter)) ->
        // Where to look, and what the file becomes when it becomes a different length. Both exact,
        // where a count of the lines that differ by position is not a count of edits and read as
        // nonsense the moment one line was split into several.
        let says: string =
            if lineCountAfter = lineCount then
                $"Not formatted: the first change is at line %d{firstChangedLine}."
            else
                $"Not formatted: the first change is at line %d{firstChangedLine}, and the file would go from %s{describeLines lineCount} to %d{lineCountAfter}."

        doctorRow glyphs.NeedsFormatting "Format" says

let describeDoctorValidity (glyphs: StatusGlyphs) (step: ValidityStep) : DoctorRow =
    match step with
    | ValidityStep.Valid -> doctorRow glyphs.Formatted "Valid" "Fantomas accepts what it produced."
    | ValidityStep.Invalid _ ->
        doctorRow glyphs.Errored "Valid" "Fantomas will not accept what it produced, so nothing would be written."

let describeDoctorIdempotency (theme: Theme) (glyphs: StatusGlyphs) (step: IdempotencyStep) : DoctorRow =
    match step with
    | IdempotencyStep.Idempotent ->
        doctorRow glyphs.Formatted "Idempotent" "Formatting the result again changes nothing."
    | IdempotencyStep.Failed error ->
        doctorRow glyphs.Errored "Idempotent" $"Formatting the result again failed: %s{error.Message}"
    | IdempotencyStep.NotIdempotent(line, afterFirst, afterSecond) ->
        { doctorRow glyphs.Errored "Idempotent" $"Formatting the result again changes it, first at line %d{line}." with
            Detail =
                [
                    // `after one pass:` is two characters shorter than `after two passes:`, so
                    // padded to the longer of them the two lines can be read against each other,
                    // which is the whole reason both are printed.
                    String.Concat("after one pass:   ", placeholder theme afterFirst)
                    String.Concat("after two passes: ", placeholder theme afterSecond)
                ]
        }

/// Why the walk did not reach the steps it did not reach. Read off where it stopped rather than
/// carried, so a step that gains a way to stop the walk cannot forget to say which one it was.
let doctorStoppedBecause (report: DoctorReport) : string =
    match report.File, report.Ignore, report.Format, report.Validity with
    | FileStep.NotFound _, _, _, _ -> "there is no file here to put through them"
    | FileStep.NotFSharp _, _, _, _ -> "Fantomas does not format this kind of file"
    | _, Some(IgnoreStep.Governed(_, true, _)), _, _ -> "Fantomas does not format a file its .fantomasignore matches"
    | _, _, Some(FormatStep.Failed _), _ -> "formatting produced nothing to look at"
    | _, _, _, Some(ValidityStep.Invalid _) -> "Fantomas would not accept what formatting produced"
    | _ -> "the walk stopped before them"

let reportDoctorReport (env: CliEnvironment) (settings: CliSettings) (report: DoctorReport) : unit =
    let theme: Theme = env.OutputTheme
    let glyphs: StatusGlyphs = statusGlyphs theme
    let verbose: bool = settings.Verbosity = VerbosityLevel.Detailed

    let path: string =
        match report.File with
        | FileStep.Candidate file -> file.Path
        | FileStep.NotFound path
        | FileStep.NotFSharp path -> path

    let lineCount: int =
        match report.File with
        | FileStep.Candidate file -> file.LineCount
        | FileStep.NotFound _
        | FileStep.NotFSharp _ -> 0

    // A step that was reached becomes a row; a step that was not becomes a name in one sentence
    // below the table. Five muted rows saying nothing happened is not a report of a file that is
    // not there, and leaving them out entirely would leave the reader to notice the absence.
    let rows: ResizeArray<DoctorRow> = ResizeArray()
    let skipped: ResizeArray<string> = ResizeArray()

    let step (label: string) (describe: 'step -> DoctorRow) (reached: 'step option) : unit =
        match reached with
        | Some reached -> rows.Add(describe reached)
        | None -> skipped.Add label

    rows.Add(describeDoctorFile theme glyphs report.File)
    step "Ignore" (describeDoctorIgnore theme glyphs verbose) report.Ignore
    step "Settings" (describeDoctorSettings theme glyphs) report.Settings
    step "Format" (describeDoctorFormat glyphs lineCount) report.Format
    step "Valid" (describeDoctorValidity glyphs) report.Validity
    step "Idempotent" (describeDoctorIdempotency theme glyphs) report.Idempotency

    let column: int = doctorColumn (List.ofSeq rows)
    let mutable lastWasBlank: bool = false

    let write (line: string) : unit =
        env.Log.Information line
        lastWasBlank <- String.IsNullOrEmpty line

    // One blank line, however many are asked for in a row. Every block here opens and closes with
    // one so that it reads as a block, and two blocks meeting would otherwise leave a gap twice the
    // size of the ones inside them.
    let blank () : unit =
        if not lastWasBlank then
            write ""

    // The whole version, commit hash and all, where every other page trims it to the short form.
    // This report is what gets pasted into a bug report, and the build that produced it is the
    // first thing whoever reads it has to know: a trimmed hash is one they have to ask back for.
    write (String.Concat(title theme "Fantomas", " ", CodeFormatter.GetVersion(), " on ", link theme path))

    blank ()

    for row in rows do
        writeRow write column (String.Concat(row.Glyph, " ", row.Label)) row.Says

        // A blank line either side of the detail, and the detail in the same column as the line it
        // hangs under. Indenting it further made it a second table inside the first, and what it
        // holds is the working out behind the sentence above it rather than something subordinate
        // to it. Without the closing blank the last line of it runs straight into the next step.
        if not (List.isEmpty row.Detail) then
            blank ()

            for detail in row.Detail do
                if String.IsNullOrEmpty detail then
                    blank ()
                else
                    writeContinuation write column detail

            blank ()

    if skipped.Count > 0 then
        let names: string list = List.ofSeq skipped
        let were: string = plural (List.length names) "was" "were"

        blank ()
        write (muted theme $"%s{andList names} %s{were} not looked at: %s{doctorStoppedBecause report}.")

    // What a failure has to say for itself goes below the table at full width, because a parse
    // failure draws a snippet with a caret under it and an indented block of source is a block
    // nobody can line up against their file.
    let source () : string = sourceOf env.FileSystem path

    let footer: string option =
        match report.Format, report.Validity with
        | Some(FormatStep.Failed error), _ -> describeItself theme path source verbose error
        | Some(FormatStep.Produced(formatted, _)), Some(ValidityStep.Invalid diagnostics) ->
            Some(Diagnostics.renderInvalidOutput theme path formatted diagnostics)
        | _ -> None

    match footer with
    | None -> ()
    | Some report ->
        blank ()
        write report

let reportDoctorCommand (env: CliEnvironment) (settings: CliSettings) (result: DoctorCommandResult) : int =
    match result with
    | DoctorCommandResult.Failed error -> env.Log.Error $"%s{error.Message}"
    | DoctorCommandResult.Completed report -> reportDoctorReport env settings report
    | DoctorCommandResult.NotOneFile given ->
        // Every other command takes any number of files and folders, so this is the mistake to
        // expect rather than one to be terse about. Both ways of making it get the command that
        // answers the question the reader was really asking.
        //
        // The error theme, because this lands on standard error: it is the one thing this command
        // says that is not part of the report, and there is no report for it to be out of order in.
        let theme: Theme = env.ErrorTheme

        let says: string =
            match given with
            | InputPath.Folder folder ->
                String.Concat(
                    "doctor reports on one file, and ",
                    folder,
                    " is a folder. Name a file inside it, or run ",
                    muted theme env.Invocation,
                    flagName theme (String.Concat(" check ", folder)),
                    " to find out what the whole tree comes to."
                )
            | InputPath.Multiple(files, folders) ->
                let count: int = List.length files + List.length folders

                $"doctor reports on one file, and %d{count} paths were given. Name one of them."
            | InputPath.File _
            | InputPath.NoFSharpFile _
            | InputPath.NotFound _ -> "doctor reports on one file."

        env.Log.Error says

    result.ExitCode
