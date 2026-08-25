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
