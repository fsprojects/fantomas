module Fantomas.EditorConfigReport

open System
open System.Collections.Concurrent
open Serilog
open Serilog.Events
open Fantomas.Core
open Fantomas.EditorConfig

type EditorConfigReporter = string -> EditorConfigProblem list -> unit

let fantomasVersion: string =
    let version = CodeFormatter.GetVersion()

    match version.IndexOf('+') with
    | -1 -> version
    | index -> version.Substring(0, index)

/// How many single character edits turn one setting name into the other, capped at `limit` so a
/// name nothing like any of ours stops being measured early rather than walking every candidate
/// to the end.
let editDistance (limit: int) (left: string) (right: string) : int =
    if abs (left.Length - right.Length) > limit then
        limit + 1
    else
        let mutable previous = Array.init (right.Length + 1) id
        let mutable current = Array.zeroCreate<int> (right.Length + 1)

        for row in 1 .. left.Length do
            current[0] <- row

            for column in 1 .. right.Length do
                let substitution =
                    previous[column - 1] + (if left[row - 1] = right[column - 1] then 0 else 1)

                current[column] <- min (min (current[column - 1] + 1) (previous[column] + 1)) substitution

            let swap = previous
            previous <- current
            current <- swap

        min previous[right.Length] (limit + 1)

/// Close enough that naming the other one is help rather than noise. Three edits is roughly a
/// doubled letter, a dropped one and a swapped pair; beyond that the guess is worse than silence.
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
    | _ ->
        supportedSettings
        |> List.choose (fun candidate ->
            match editDistance MaximumSuggestionDistance setting candidate with
            | distance when distance <= MaximumSuggestionDistance -> Some(candidate, distance)
            | _ -> None)
        |> List.sortBy snd
        |> List.tryHead
        |> Option.map fst

/// Settings and values are quoted the way the rest of the tool quotes what it was given, and for
/// the same reason: both are text someone else wrote. A value can be empty, or carry spaces, or
/// read like prose, and unquoted it runs into the sentence around it.
let describeProblem (problem: EditorConfigProblem) : string =
    match problem with
    | EditorConfigProblem.UnknownSetting setting ->
        match suggestionFor setting with
        | Some suggestion -> $"  '%s{setting}' is not a Fantomas setting. Did you mean '%s{suggestion}'?"
        | None -> $"  '%s{setting}' is not a Fantomas setting."
    | EditorConfigProblem.UnrecognizedValue(setting, value) ->
        $"  '%s{setting}' does not accept the value '%s{value}', so the default is used instead."

let describe (origin: string) (problems: EditorConfigProblem list) : string option =
    if List.isEmpty problems then
        None
    else
        let namesAnyUnknownSetting =
            problems
            |> List.exists (fun problem ->
                match problem with
                | EditorConfigProblem.UnknownSetting _ -> true
                | EditorConfigProblem.UnrecognizedValue _ -> false)

        [ yield ""
          yield $"Fantomas cannot use some settings from %s{origin}:"
          for problem in problems do
              yield describeProblem problem
          if namesAnyUnknownSetting then
              yield
                  $"Run fantomas with --verbosity d to see every .editorconfig setting fantomas %s{fantomasVersion} supports."
          yield "" ]
        |> String.concat Environment.NewLine
        |> Some

let describeSupportedSettings () : string =
    [ yield ""
      yield $"fantomas %s{fantomasVersion} supports these .editorconfig settings:"
      for setting in supportedSettings do
          yield $"  %s{setting}"
      yield "" ]
    |> String.concat Environment.NewLine

let createReporter (log: ILogger) : EditorConfigReporter =
    // Files are formatted in parallel, so more than one of them can arrive here at once. The whole
    // report is one message rather than a line each: written as several, another thread's report
    // can land in the middle of this one.
    let reported = ConcurrentDictionary<string, unit>()
    let supportedSettingsWritten = ref 0

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
            | [] -> fsharpFile
            | files -> String.concat ", " files

        report origin result.Problems
        result.Config
