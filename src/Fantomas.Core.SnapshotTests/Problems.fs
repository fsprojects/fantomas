/// Everything that can be wrong with a case, and how a failing test words it.
module Fantomas.Core.SnapshotTests.Problems

open Fantomas.FCS.Parse
open Fantomas.FCS.Text
open Fantomas.Core

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
    | CommentsNotFound of comments: string list
    | Invalid of output: Output * diagnostics: FSharpParserDiagnostic list
    // Stricter than what users are refused output for, through `Validations.TriviaComparison`.
    // `defines` is the combination the source and the result were both read under, when the source
    // has `#if`: what is in an inactive branch is only trivia under the defines that make it active.
    | CommentsChanged of defines: string list option * missing: string list * added: string list
    | DirectivesChanged of defines: string list option * missing: string list * added: string list
    | NotIdempotent of output: Output * again: string
    | CheckFailed of check: Validations * error: exn
    // An Oak trivia assignment cannot rely on, as formatting handed it over: the one printed for
    // `defines`, or for the result formatted again.
    | ChildrenOutOfPlace of defines: string list * secondPass: bool * children: string list
    | TriviaNotAttached of defines: string list * secondPass: bool * trivia: string list
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
    | GoldLineEndings of path: string * lineEnding: string

let private outputName (output: Output) : string =
    match output with
    | Output.Merged -> "The result"
    | Output.Combination defines -> $"The result for %s{Case.combinationName defines}"

let private oakName (defines: string list) (secondPass: bool) : string =
    let pass: string = if secondPass then " of the result formatted again" else ""

    $"The Oak%s{pass} for %s{Case.combinationName defines}"

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

// One comment a paragraph, as a comment can span lines.
let private comments (texts: string list) : string =
    match texts with
    | [] -> " none"
    | texts -> texts |> List.map (fun (text: string) -> $"\n%s{text}") |> String.concat ""

/// A problem as the failing test words it.
let describe (problem: Problem) : string =
    match problem with
    | Problem.CommentsNotFound missing ->
        $"A format run does not find every comment of the source in the result, and would refuse it:%s{comments missing}"
    | Problem.Invalid(output, diagnostics) -> $"%s{outputName output} is not valid F#:\n%s{diagnosticLines diagnostics}"
    | Problem.CommentsChanged(defines, missing, added) ->
        $"Comments were not preserved%s{under defines}.\nMissing:%s{comments missing}\nExtra:%s{comments added}"
    | Problem.DirectivesChanged(defines, [], []) ->
        $"Conditional and warn directives are all there and in another order%s{under defines}. Merging the define combinations relies on their order."
    | Problem.DirectivesChanged(defines, missing, added) ->
        $"Conditional and warn directives were not preserved%s{under defines}.\nMissing: %s{listed missing}\nExtra: %s{listed added}"
    | Problem.NotIdempotent(output, again) ->
        $"%s{outputName output} is not idempotent. Formatting it again gave:\n%s{again}"
    | Problem.CheckFailed(check, error) -> $"The %O{check} check failed on the result:\n%s{error.Message}"
    | Problem.ChildrenOutOfPlace(defines, secondPass, children) ->
        let listed: string = String.concat "\n" children
        $"%s{oakName defines secondPass} has children out of place:\n%s{listed}"
    | Problem.TriviaNotAttached(defines, secondPass, trivia) ->
        let listed: string = String.concat "\n" trivia
        $"%s{oakName defines secondPass} has trivia the parser recorded attached to no node:\n%s{listed}"
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
    | Problem.GoldLineEndings(path, lineEnding) ->
        $"%s{path} has line endings formatting never gives here, where every line ends in `%s{lineEnding}`. An editor that saved it with its own would do this: it could never match."
