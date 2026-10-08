/// The tests: one per case, and one for the files around the cases.
module Fantomas.Core.SnapshotTests.CaseTests

open System
open System.IO
open Microsoft.FSharp.Reflection
open NUnit.Framework
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.EditorConfig
open Fantomas.Core.SnapshotTests.Problems

[<assembly: Parallelizable(ParallelScope.All)>]
do ()

let cases () : string array = Case.all ()

/// Fail with every problem, worded.
let private failWith (problems: Problem list) : unit =
    if not problems.IsEmpty then
        Assert.Fail(problems |> List.map describe |> String.concat "\n\n")

/// Whether a case is under `negative/`, and so its own gold.
let private isNegative (case: Case.Case) : bool =
    match Placement.claimOf case with
    | Error _ -> false
    | Ok claim -> claim.IsNegative

/// The golds a case has to match, with what each must hold. A case under `negative/` is its own gold.
/// Its per-define golds stay: what each combination printed is not its input.
let private goldsOf (case: Case.Case) (formatted: Formatting.Formatted) : (string * string) list =
    let perDefine: (string * string) list =
        match formatted.Combinations with
        | [ _ ] -> []
        | combinations ->

        combinations
        |> List.map (fun (each: Formatting.FormattedCombination) -> Case.defineGoldPath case each.Defines, each.Code)

    if isNegative case then
        perDefine
    else
        (Case.goldPath case, formatted.Merged) :: perDefine

/// An ignored case is skipped with its reason while it does not produce its golds, and fails once it
/// does, so that it loses its `.ignore`. Only the golds it has are compared: they hold what it should
/// give, written by hand, and nothing writes them for it.
let private ignored (case: Case.Case) : unit =
    if case.Description.IsEmpty then
        Assert.Fail "An ignored case says why in a `#` description."

    if not (isNegative case) && not (File.Exists(Case.goldPath case)) then
        Assert.Fail
            $"An ignored case needs the gold it should produce: %s{Case.relativeToProject (Case.goldPath case)}."

    // What the folders ask of the input alone holds whatever formatting gives, even when it throws.
    failWith (Placement.inputProblems case)

    failWith (
        Case.existingGolds case
        |> List.choose (Gold.lineEndingsProblem case.Config.EndOfLine)
    )

    // An ignored case under `negative/` is its own gold as much as any other.
    if isNegative case && File.Exists(Case.goldPath case) then
        failWith [ Problem.GoldNotExpected(Case.relativeToProject (Case.goldPath case)) ]

    let passes, standing, thrown =
        try
            let formatted, resultProblems =
                Formatting.formatAndCheck case.Config case.IsSignature case.Source

            let placementProblems: Problem list =
                Placement.check
                    case
                    formatted
                    (fun config -> (Formatting.formatEach config case.IsSignature case.Source).Merged)

            let mismatched: (string * string) list =
                goldsOf case formatted
                |> List.filter (fun (path: string, code: string) -> File.Exists path && not (Gold.holds path code))

            // What it gives today, beside the gold it should give.
            for path, code in goldsOf case formatted do
                if List.exists (fun (mismatch: string, _) -> mismatch = path) mismatched then
                    File.WriteAllText(Gold.actualPath path, code)
                else
                    File.Delete(Gold.actualPath path)

            // The result is what is known to be wrong, so only what holds whatever it is gets reported
            // while the case is ignored: the node the folder names, which the input's Oak has or not,
            // and a gold for a define combination the input does not have. Both need the case to
            // format; one that throws is skipped without them.
            let missingNodes: Problem list =
                placementProblems
                |> List.filter (fun (problem: Problem) ->
                    match problem with
                    | Problem.NodeMissing _ -> true
                    | _ -> false
                )

            let staleGolds: Problem list =
                Case.existingGolds case
                |> List.choose (fun (path: string) ->
                    if List.exists (fun (gold: string, _) -> gold = path) (goldsOf case formatted) then
                        None
                    else
                        Some(Problem.StaleGold(Case.relativeToProject path))
                )

            resultProblems.IsEmpty && placementProblems.IsEmpty && mismatched.IsEmpty, missingNodes @ staleGolds, None
        with ex ->
            false, [], Some ex.Message

    failWith standing

    if passes then
        Assert.Fail $"%s{case.RelativePath} gives its golds now: rename it to %s{case.Stem}%s{case.Extension}."

    // What it throws goes with the reason, so that a harness bug is not taken for the bug the case
    // is ignored for.
    match thrown with
    | None -> Assert.Ignore(String.concat " " case.Description)
    | Some message -> Assert.Ignore $"""%s{String.concat " " case.Description} It throws: %s{message}"""

[<TestCaseSource(nameof cases)>]
let case (relativePath: string) =
    // A case that cannot be read, its front matter wrong say, fails with what is wrong and no trace.
    let case: Case.Case =
        try
            Case.read relativePath
        with ex ->
            raise (AssertionException ex.Message)

    // `name.fs` and `name.ignore.fs` would share their golds and `.actual` files, so both fail
    // before either writes one.
    let twin: string =
        let name: string =
            if case.IsIgnored then
                case.Stem + case.Extension
            else
                case.Stem + Case.ignoreSuffix + case.Extension

        Path.Combine(Path.GetDirectoryName case.FullPath, name)

    if File.Exists twin then
        Assert.Fail $"Both %s{case.RelativePath} and %s{Case.relativeToCases twin} exist."

    if case.IsIgnored then
        ignored case
    else

    let formatted, resultProblems =
        Formatting.formatAndCheck case.Config case.IsSignature case.Source

    let placementProblems: Problem list =
        Placement.check
            case
            formatted
            (fun config -> (Formatting.formatEach config case.IsSignature case.Source).Merged)

    let isNegative: bool = isNegative case
    let golds: (string * string) list = goldsOf case formatted

    // A result that earns no gold is reported as that, and not as a gold that is missing or differs too.
    let earnsNoGold: bool =
        placementProblems
        |> List.exists (fun (problem: Problem) ->
            match problem with
            | Problem.AlreadyFormatted
            | Problem.OnlyEndChanged -> true
            | _ -> false
        )

    // Updating leaves the golds alone while the result is itself wrong, which would be compared
    // against from then on, or while the case is not where it belongs, which is what to fix first.
    // Every problem `formatAndCheck` finds is one with the result.
    let goldProblems: Problem list =
        if Case.isUpdating && (not resultProblems.IsEmpty || not placementProblems.IsEmpty) then
            []
        else

        // A gold for a define combination the case no longer has, or any gold of a case that is its own.
        let stale: Problem list =
            Case.existingGolds case
            |> List.filter (fun (path: string) -> not (List.exists (fun (gold: string, _) -> gold = path) golds))
            |> List.choose (fun (path: string) ->
                if not Case.isUpdating then
                    if isNegative && path = Case.goldPath case then
                        Some(Problem.GoldNotExpected(Case.relativeToProject path))
                    else
                        Some(Problem.StaleGold(Case.relativeToProject path))
                else

                File.Delete path
                None
            )

        stale
        @ (golds
           |> List.choose (fun (path: string, code: string) ->
               if earnsNoGold && path = Case.goldPath case then
                   // An `.actual` an earlier run left beside it would no longer be what came out.
                   File.Delete(Gold.actualPath path)
                   None
               else
                   Gold.verify path code
           ))

    failWith (resultProblems @ placementProblems @ goldProblems)

/// Every file under `cases/` is a case, a gold of one, an `.actual` of one, or a `README.md`. An
/// `.actual` of no case is deleted instead.
[<Test>]
let ``every file under cases belongs to a case`` () =
    let strays: string list =
        if not (Directory.Exists Case.casesDirectory) then
            []
        else

        Directory.GetFiles(Case.casesDirectory, "*", SearchOption.AllDirectories)
        // A hidden file, such as the `.DS_Store` macOS leaves in a folder, is no one's to place.
        |> Array.filter (fun (path: string) -> not (Path.GetFileName(path).StartsWith('.')))
        |> Array.choose (fun (path: string) ->
            let name: string = Path.GetFileName path

            let stem: string =
                match name.IndexOf '.' with
                | -1 -> name
                | dot -> name.Substring(0, dot)

            let extension: string = Path.GetExtension path

            let isGold: bool = name.Contains(".gold.", StringComparison.Ordinal)
            let isActual: bool = name.Contains(".actual.", StringComparison.Ordinal)

            let hasCase: bool =
                File.Exists(Path.Combine(Path.GetDirectoryName path, stem + extension))
                || File.Exists(Path.Combine(Path.GetDirectoryName path, stem + Case.ignoreSuffix + extension))

            // A `README.md` tells what the cases of its folder share: their history, or a rule.
            if Case.isCaseFile path || (isGold || isActual) && hasCase || name = "README.md" then
                None
            // Git ignores an `.actual`, so one a case renamed or deleted since left behind is
            // invisible to `git status` and `git clean`. It is cleared rather than reported.
            elif isActual then
                File.Delete path
                None
            else
                Some(Case.relativeToCases path)
        )
        |> Array.toList

    if not strays.IsEmpty then
        Assert.Fail($"""These files belong to no case:%s{"\n"}%s{String.concat "\n" strays}""")

/// The folders under `oak/` no case can fill, as `cases/oak/README.md` lists them, and why. One that
/// gains a case after all fails the test below, so the list and the README stay true.
let private nodesWithoutCase: Map<string, string> =
    Map.ofList
        [
            "TypeConstraint/DefaultsToType", "only FSharp.Core may write `default 'T : int`"
            "ExprConstant", "ASTTransformer builds no ExprConstantNode"
            "String", "ASTTransformer builds no StringNode"
            "Oak", "it is the root of every case"
        ]

/// Every union case of the Oak has a folder under `oak/` with a case in it, and so does every node
/// class no union case holds; every setting has one under `settings/`. A node or a setting added
/// later fails this until it has its first case.
[<Test>]
let ``every node and every setting has a case`` () =
    let hasCase (folder: string) : bool =
        let path: string = Path.Combine(Case.casesDirectory, folder)

        Directory.Exists path
        && Directory.GetFiles(path, "*", SearchOption.AllDirectories)
           |> Array.exists Case.isCaseFile

    // `TriviaContent` is what trivia is, and no node of the tree.
    let unions: System.Type array =
        OakFacts.syntaxOakTypes
        |> Array.filter (fun (t: System.Type) -> FSharpType.IsUnion(t, true) && t <> typeof<TriviaContent>)

    let unionCases: UnionCaseInfo list =
        unions
        |> Array.collect (fun (union: System.Type) -> FSharpType.GetUnionCases(union, true))
        |> Array.toList

    let heldByUnionCase: Collections.Generic.HashSet<System.Type> =
        unionCases
        |> List.collect (fun (case: UnionCaseInfo) ->
            case.GetFields()
            |> Array.map (fun (field: Reflection.PropertyInfo) -> field.PropertyType)
            |> Array.toList
        )
        |> Collections.Generic.HashSet<System.Type>

    let nodeFolders: string list =
        (unionCases
         |> List.map (fun (case: UnionCaseInfo) -> $"%s{case.DeclaringType.Name}/%s{case.Name}"))
        @ (OakFacts.nodeClasses
           |> List.choose (fun (nodeClass: System.Type) ->
               if heldByUnionCase.Contains nodeClass then
                   None
               elif nodeClass.Name.EndsWith("Node", StringComparison.Ordinal) then
                   Some(nodeClass.Name.Substring(0, nodeClass.Name.Length - "Node".Length))
               else
                   Some nodeClass.Name
           ))

    let missingNodes: string list =
        nodeFolders
        |> List.choose (fun (folder: string) ->
            if nodesWithoutCase.ContainsKey folder || hasCase $"oak/%s{folder}" then
                None
            else
                Some $"oak/%s{folder}/"
        )

    let missingSettings: string list =
        FSharpType.GetRecordFields(typeof<FormatConfig>)
        |> Array.choose (fun (field: Reflection.PropertyInfo) ->
            let folder: string = $"settings/%s{toEditorConfigName field.Name}/"
            if hasCase folder then None else Some folder
        )
        |> Array.toList

    let filledAfterAll: string list =
        nodesWithoutCase
        |> Map.toList
        |> List.choose (fun (folder: string, reason: string) ->
            if not (hasCase $"oak/%s{folder}") then
                None
            else
                Some
                    $"oak/%s{folder}/ has a case, and is listed as a folder no case can fill, since %s{reason}: take it off the list in CaseTests.fs and in cases/oak/README.md."
        )

    let missing: string list =
        match missingNodes @ missingSettings with
        | [] -> []
        | missing -> [ $"""These folders have no case:%s{"\n"}%s{String.concat "\n" missing}""" ]

    match missing @ filledAfterAll with
    | [] -> ()
    | problems -> Assert.Fail(String.concat "\n\n" problems)
