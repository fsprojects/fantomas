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
    | CommentsLost of missing: Set<TriviaContent> * extra: Set<TriviaContent>
    | CommentCountChanged of before: int * after: int
    | NotIdempotent of output: Output * again: string
    | TrailingWhitespace of output: Output * lines: int list
    // The case is in the wrong place.
    | UnknownFolder of reason: string
    | SettingNotSet of key: string
    | SettingValueDiffers of key: string * folderValue: string * written: string
    | SettingHasNoEffect of key: string
    | NodeMissing of nodeClass: string
    | TriviaNotAttached of nodeClass: string
    // The gold disagrees. Paths are relative to the project.
    | NoGold of path: string
    | GoldDiffers of path: string * diff: string
    | StaleGold of path: string

/// Whether a problem says the result itself is wrong. Such a result is never written as a gold,
/// not even when updating them.
let breaksResult (problem: Problem) : bool =
    match problem with
    | Problem.ProductionDiffers _
    | Problem.Invalid _
    | Problem.CommentsLost _
    | Problem.CommentCountChanged _
    | Problem.NotIdempotent _
    | Problem.TrailingWhitespace _ -> true
    | Problem.UnknownFolder _
    | Problem.SettingNotSet _
    | Problem.SettingValueDiffers _
    | Problem.SettingHasNoEffect _
    | Problem.NodeMissing _
    | Problem.TriviaNotAttached _
    | Problem.NoGold _
    | Problem.GoldDiffers _
    | Problem.StaleGold _ -> false

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

/// A problem as the failing test words it.
let describe (problem: Problem) : string =
    match problem with
    | Problem.ProductionDiffers(production, harness) ->
        $"The harness and `formatDocumentWith` disagree. Production gave:\n%s{production}\nThe harness gave:\n%s{harness}"
    | Problem.Invalid(output, diagnostics) -> $"%s{outputName output} is not valid F#:\n%s{diagnosticLines diagnostics}"
    | Problem.CommentsLost(missing, extra) -> $"Comments were not preserved.\nMissing: %A{missing}\nExtra: %A{extra}"
    | Problem.CommentCountChanged(before, after) -> $"The source has %d{before} comments and the result %d{after}."
    | Problem.NotIdempotent(output, again) ->
        $"%s{outputName output} is not idempotent. Formatting it again gave:\n%s{again}"
    | Problem.TrailingWhitespace(output, lines) ->
        let numbers: string =
            lines |> List.map (fun (line: int) -> $"%d{line}") |> String.concat ", "

        $"%s{outputName output} has trailing whitespace on line %s{numbers}."
    | Problem.UnknownFolder reason -> reason
    | Problem.SettingNotSet key -> $"The case is under `settings/%s{key}` and its front matter does not set `%s{key}`."
    | Problem.SettingValueDiffers(key, folderValue, written) ->
        $"The case is under `%s{folderValue}` and its front matter sets `%s{key} = %s{written}`."
    | Problem.SettingHasNoEffect key ->
        $"`%s{key}` changes nothing here: with it at its default the result is the same."
    | Problem.NodeMissing nodeClass -> $"The case is in a folder for `%s{nodeClass}` and its Oak has none."
    | Problem.TriviaNotAttached nodeClass ->
        $"The case is in the `trivia/` folder of `%s{nodeClass}` and no trivia is attached to one, or to one of its direct children."
    | Problem.NoGold path -> $"There is no gold at %s{path} yet. What came out is in its `.actual` file."
    | Problem.GoldDiffers(path, diff) ->
        $"The result differs from %s{path}. What came out is in its `.actual` file.\n\n%s{diff}"
    | Problem.StaleGold path -> $"%s{path} belongs to no define combination this case has."
