/// Everything that can be wrong with a case, and how a failing test words it.
module Fantomas.Core.SnapshotTests.Problems

open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.Core.SyntaxOak

/// Which of a case's results a problem is about.
[<RequireQualifiedAccess>]
type Output =
    /// What users get: the merged result, or the only one when the case has no `#if`.
    | Merged
    /// What one define combination printed before the merge.
    | Combination of defines: string list

[<RequireQualifiedAccess; NoComparison>]
type Problem =
    // The result itself is wrong.
    | ProductionDiffers of production: string * harness: string
    | Invalid of output: Output * diagnostics: FSharpParserDiagnostic list
    // `defines` is the combination the source and the result were both read under, when the source
    // has `#if`: what is in an inactive branch is only trivia under the defines that make it active.
    | CommentsLost of defines: string list option * missing: Set<TriviaContent> * extra: Set<TriviaContent>
    | CommentCountChanged of defines: string list option * before: int * after: int
    | DirectivesChanged of defines: string list option * missing: string list * extra: string list
    | NotIdempotent of output: Output * again: string
    | CrlfDiffers of crlf: string
    | TrailingWhitespace of output: Output * lines: int list
    // The case is in the wrong place.
    | UnknownFolder of reason: string
    | SettingNotSet of key: string
    | SettingValueDiffers of key: string * folderValue: string * written: string
    | SettingAtDefault of key: string
    | SettingHasNoEffect of key: string * otherValues: string list
    | InputNotKept of formatted: string
    | AlreadyFormatted
    | OnlyEndChanged
    | SettingApplies of key: string * otherValue: string * result: string
    | NodeMissing of nodeClass: string
    // The gold disagrees. Paths are relative to the project.
    | NoGold of path: string
    | GoldDiffers of path: string * diff: string
    | StaleGold of path: string
    | GoldNotExpected of path: string

let private outputName (output: Output) : string =
    match output with
    | Output.Merged -> "The result"
    | Output.Combination defines -> $"The result for %s{Case.combinationName defines}"

let private diagnosticLines (diagnostics: FSharpParserDiagnostic list) : string =
    diagnostics
    |> List.map (fun (diagnostic: FSharpParserDiagnostic) ->
        let at: string =
            diagnostic.Range
            |> Option.map (fun (range: range) -> $"(%d{range.StartLine},%d{range.StartColumn}) ")
            |> Option.defaultValue ""

        $"%s{at}%s{diagnostic.Message}"
    )
    |> String.concat "\n"

let private under (defines: string list option) : string =
    match defines with
    | None -> ""
    | Some defines -> $" (read under %s{Case.combinationName defines})"

let private listed (directives: string list) : string =
    match directives with
    | [] -> "none"
    | directives ->

    directives
    |> List.map (fun (directive: string) -> $"`%s{directive}`")
    |> String.concat ", "

/// A problem as the failing test words it.
let describe (problem: Problem) : string =
    match problem with
    | Problem.ProductionDiffers(production, harness) ->
        $"The harness and `formatDocumentWith` disagree. Production gave:\n%s{production}\nThe harness gave:\n%s{harness}"
    | Problem.Invalid(output, diagnostics) -> $"%s{outputName output} is not valid F#:\n%s{diagnosticLines diagnostics}"
    | Problem.CommentsLost(defines, missing, extra) ->
        $"Comments were not preserved%s{under defines}.\nMissing: %A{missing}\nExtra: %A{extra}"
    | Problem.CommentCountChanged(defines, before, after) ->
        $"The source has %d{before} comments and the result %d{after}%s{under defines}."
    | Problem.DirectivesChanged(defines, missing, extra) ->
        $"Conditional and warn directives were not preserved%s{under defines}.\nMissing: %s{listed missing}\nExtra: %s{listed extra}"
    | Problem.NotIdempotent(output, again) ->
        $"%s{outputName output} is not idempotent. Formatting it again gave:\n%s{again}"
    | Problem.CrlfDiffers crlf ->
        let shown: string = crlf.Replace("\r", "\\r")

        $"With `\\r\\n` line endings in and `end_of_line = crlf`, the result is not this one with `\\r\\n` line endings. It gave, line endings shown:\n%s{shown}"
    | Problem.TrailingWhitespace(output, lines) ->
        let numbers: string =
            lines |> List.map (fun (line: int) -> $"%d{line}") |> String.concat ", "

        $"%s{outputName output} has trailing whitespace on line %s{numbers}."
    | Problem.UnknownFolder reason -> reason
    | Problem.SettingNotSet key -> $"The case is under `settings/%s{key}` and its front matter does not set `%s{key}`."
    | Problem.SettingValueDiffers(key, folderValue, written) ->
        $"The case is under `settings/%s{key}/%s{folderValue}/` and its front matter sets `%s{key} = %s{written}`."
    | Problem.SettingAtDefault key ->
        $"The case is under `settings/%s{key}` and its front matter sets `%s{key}` to its default, which says nothing about the setting."
    | Problem.SettingHasNoEffect(key, otherValues) ->
        let values: string =
            otherValues
            |> List.map (fun (value: string) -> $"`%s{value}`")
            |> String.concat " or "

        $"`%s{key}` changes nothing here: at %s{values} the result is the same."
    | Problem.InputNotKept formatted ->
        $"The case is under `negative/`, so it is its own gold and formatting must leave it as it is. It gave:\n%s{formatted}"
    | Problem.AlreadyFormatted ->
        "The result is the input unchanged, so a gold would only repeat it. Change the input so the result earns its gold, or move the case to a `negative/` folder, where a case is its own gold."
    | Problem.OnlyEndChanged ->
        "The result is the input with only its end changed, a final newline added say, so a gold would show nothing else. End the input the way the result does and move the case to a `negative/` folder, where a case is its own gold."
    | Problem.SettingApplies(key, otherValue, result) ->
        $"The case is under `negative/`, and `%s{key}` does change it: at `%s{otherValue}` the result is:\n%s{result}"
    | Problem.NodeMissing nodeClass -> $"The case is in a folder for `%s{nodeClass}` and its Oak has none."
    | Problem.NoGold path -> $"There is no gold at %s{path} yet. What came out is in its `.actual` file."
    | Problem.GoldDiffers(path, diff) ->
        $"The result differs from %s{path}. What came out is in its `.actual` file.\n\n%s{diff}"
    | Problem.StaleGold path -> $"%s{path} belongs to no define combination this case has."
    | Problem.GoldNotExpected path -> $"%s{path} belongs to a case under `negative/`, which is its own gold."
