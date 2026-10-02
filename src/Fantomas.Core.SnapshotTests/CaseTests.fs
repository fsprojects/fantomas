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

[<TestCaseSource(nameof cases)>]
let case (relativePath: string) =
    let case: Case.Case = Case.read relativePath

    let formatted, resultProblems =
        Formatting.formatAndCheck case.Config case.IsSignature case.Source

    let placementProblems: Problem list =
        Placement.check
            case
            formatted
            (fun config -> (Formatting.formatEach config case.IsSignature case.Source).Merged)

    // A case under `negative/` is its own gold.
    let isNegative: bool =
        match Placement.claimOf case with
        | Error _ -> false
        | Ok claim -> claim.IsNegative

    let golds: (string * string) list =
        if isNegative then
            []
        else

        let perDefine: (string * string) list =
            match formatted.Combinations with
            | [ _ ] -> []
            | combinations ->

            combinations
            |> List.map (fun (each: Formatting.ForDefines) -> Case.defineGoldPath case each.Defines, each.Code)

        (Case.goldPath case, formatted.Merged) :: perDefine

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
                    if isNegative then
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

            if Case.isCaseFile path || isGoldOrActual && hasCase then
                None
            else
                Some(Case.relativeToCases path)
        )
        |> Array.toList

    if not strays.IsEmpty then
        Assert.Fail($"""These files belong to no case:%s{"\n"}%s{String.concat "\n" strays}""")
