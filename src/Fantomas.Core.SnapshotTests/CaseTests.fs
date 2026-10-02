/// The tests: one per case, and one for the files around the cases.
module Fantomas.Core.SnapshotTests.CaseTests

open System
open System.IO
open NUnit.Framework
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
        |> List.map (fun (each: Formatting.ForDefines) -> Case.defineGoldPath case each.Defines, each.Code)

    if isNegative case then
        perDefine
    else
        (Case.goldPath case, formatted.Merged) :: perDefine

/// An ignored case is skipped with its reason while it does not produce its golds, and fails once it
/// does, so that it loses its `.ignore`. Only the golds it has are compared: they hold what it should
/// give, written by hand, and nothing writes them for it.
let private ignored (case: Case.Case) : unit =
    let twin: string =
        Path.Combine(Path.GetDirectoryName case.FullPath, case.Stem + case.Extension)

    if File.Exists twin then
        Assert.Fail $"Both %s{case.RelativePath} and %s{Case.relativeToCases twin} exist."

    if case.Description.IsEmpty then
        Assert.Fail "An ignored case says why in a `#` description."

    if not (isNegative case) && not (File.Exists(Case.goldPath case)) then
        Assert.Fail
            $"An ignored case needs the gold it should produce: %s{Case.relativeToProject (Case.goldPath case)}."

    let passes: bool =
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
                |> List.filter (fun (path: string, code: string) -> File.Exists path && File.ReadAllText path <> code)

            // What it gives today, beside the gold it should give.
            for path, code in goldsOf case formatted do
                if List.exists (fun (mismatch: string, _) -> mismatch = path) mismatched then
                    File.WriteAllText(Gold.actualPath path, code)
                else
                    File.Delete(Gold.actualPath path)

            resultProblems.IsEmpty && placementProblems.IsEmpty && mismatched.IsEmpty
        with _ ->
            false

    if passes then
        Assert.Fail $"%s{case.RelativePath} gives its golds now: rename it to %s{case.Stem}%s{case.Extension}."

    Assert.Ignore(String.concat " " case.Description)

[<TestCaseSource(nameof cases)>]
let case (relativePath: string) =
    let case: Case.Case = Case.read relativePath

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

    // A result that is itself wrong is not written as a gold, not even when updating: it would be
    // compared against from then on.
    let goldProblems: Problem list =
        if Case.isUpdating && List.exists breaksResult resultProblems then
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
        @ (golds |> List.choose (fun (path: string, code: string) -> Gold.verify path code))

    failWith (resultProblems @ placementProblems @ goldProblems)

/// Every file under `cases/` is a case, a gold of one, or an `.actual` of one.
[<Test>]
let ``every file under cases belongs to a case`` () =
    let strays: string list =
        if not (Directory.Exists Case.casesDirectory) then
            []
        else

        Directory.GetFiles(Case.casesDirectory, "*", SearchOption.AllDirectories)
        |> Array.choose (fun (path: string) ->
            let name: string = Path.GetFileName path
            let stem: string = name.Substring(0, name.IndexOf '.')
            let extension: string = Path.GetExtension path

            let isGoldOrActual: bool =
                name.Contains(".gold.", StringComparison.Ordinal)
                || name.Contains(".actual.", StringComparison.Ordinal)

            let hasCase: bool =
                File.Exists(Path.Combine(Path.GetDirectoryName path, stem + extension))
                || File.Exists(Path.Combine(Path.GetDirectoryName path, stem + Case.ignoreSuffix + extension))

            if Case.isCaseFile path || isGoldOrActual && hasCase then
                None
            else
                Some(Case.relativeToCases path)
        )
        |> Array.toList

    if not strays.IsEmpty then
        Assert.Fail($"""These files belong to no case:%s{"\n"}%s{String.concat "\n" strays}""")
