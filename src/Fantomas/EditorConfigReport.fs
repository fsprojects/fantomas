module Fantomas.EditorConfigReport

open System
open System.Collections.Concurrent
open Serilog
open Serilog.Events
open Fantomas.Core
open Fantomas.EditorConfig

type EditorConfigReporter = string -> EditorConfigProblem list -> unit

let fantomasVersion: string =
    let version: string = CodeFormatter.GetVersion()
    let buildMetadata: int = version.IndexOf('+')

    if buildMetadata = -1 then
        version
    else
        version.Substring(0, buildMetadata)

[<Literal>]
let MaximumSuggestionDistance = 3

let suggestionFor (setting: string) : string option =
    let setting = setting.ToLowerInvariant()

    // The mistake worth answering outright: the four settings editorconfig itself defines are the
    // ones a user reaches for the prefix on, and they are exactly the ones where it does not
    // belong. `fsharp_max_line_length` never applied, `max_line_length` is the one that does.
    let unprefixed =
        if isFantomasSetting setting then
            Some(setting.Substring "fsharp_".Length)
        else
            None

    match unprefixed with
    | Some unprefixed when List.contains unprefixed supportedSettings -> Some unprefixed
    | _ -> nearestSetting MaximumSuggestionDistance setting

let describeProblem (problem: EditorConfigProblem) : string =
    match problem with
    | EditorConfigProblem.UnknownSetting setting ->
        match suggestionFor setting with
        | Some suggestion -> $"'%s{setting}' is not a Fantomas setting. Did you mean '%s{suggestion}'?"
        | None -> $"'%s{setting}' is not a Fantomas setting."
    | EditorConfigProblem.UnrecognizedValue(setting, value) ->
        $"'%s{setting}' does not accept the value '%s{value}', so the default is used instead."

let describe (origin: string) (problems: EditorConfigProblem list) : string option =
    if List.isEmpty problems then
        None
    else
        let namesAnyUnknownSetting =
            problems
            |> List.exists (fun problem ->
                match problem with
                | EditorConfigProblem.UnknownSetting _ -> true
                | EditorConfigProblem.UnrecognizedValue _ -> false
            )

        [
            yield ""
            yield $"Fantomas cannot use some settings from %s{origin}:"
            for problem in problems do
                yield String.Concat("  ", describeProblem problem)
            if namesAnyUnknownSetting then
                yield
                    $"Run fantomas with --verbosity d to see every .editorconfig setting fantomas %s{fantomasVersion} supports."
            yield ""
        ]
        |> String.concat Environment.NewLine
        |> Some

let describeSupportedSettings () : string =
    [
        yield ""
        yield $"fantomas %s{fantomasVersion} supports these .editorconfig settings:"
        for setting in supportedSettings do
            yield $"  %s{setting}"
        yield ""
    ]
    |> String.concat Environment.NewLine

let createReporter (log: ILogger) : EditorConfigReporter =
    // Files are formatted in parallel, so more than one of them can arrive here at once. The whole
    // report is one message rather than a line each: written as several, another thread's report
    // can land in the middle of this one.
    let reported: ConcurrentDictionary<string, unit> =
        ConcurrentDictionary<string, unit>()

    let supportedSettingsWritten: int ref = ref 0

    fun origin problems ->
        match describe origin problems with
        | None -> ()
        | Some report ->

        if reported.TryAdd(report, ()) then
            // The report carries what someone wrote in their `.editorconfig`, so it travels as
            // a property rather than as the message template: a `{` in a value would otherwise
            // be read as the start of one.
            log.Warning("{EditorConfigReport}", report)

        if
            log.IsEnabled LogEventLevel.Debug
            && Threading.Interlocked.Exchange(&supportedSettingsWritten.contents, 1) = 0
        then
            log.Debug("{SupportedEditorConfigSettings}", describeSupportedSettings ())

let readConfiguration (report: EditorConfigReporter) (fsharpFile: string) : FormatConfig =
    match tryReadConfiguration fsharpFile with
    | None -> FormatConfig.Default
    | Some result ->

    let origin =
        match result.EditorConfigFiles with
        // The editorconfig library only reports settings it read from a file, so it should
        // always name at least one. Keep a way through anyway, because that is its invariant
        // and not ours, but do not name the F# file as the origin: nothing is wrong with it,
        // and pointing at it sends someone looking in the wrong place.
        | [] -> $"the .editorconfig that applies to %s{fsharpFile}"
        | files -> String.concat ", " files

    report origin result.Problems
    result.Config
