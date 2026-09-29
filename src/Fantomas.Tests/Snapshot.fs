/// Output compared with a file that holds what it was last time.
module Fantomas.Tests.Snapshot

open System
open System.IO
open NUnit.Framework
open Fantomas.Tests.TestHelpers

/// One line of a diff, with its line number on the side or sides it appears on. Zero-based.
type private DiffLine =
    | Same of expected: int * actual: int * line: string
    | Removed of expected: int * line: string
    | Added of actual: int * line: string

/// The plain longest common subsequence diff, for a stretch with no line that could anchor it.
let private commonSubsequenceDiff
    (expected: string array)
    (actual: string array)
    (expectedStart: int)
    (actualStart: int)
    : DiffLine list
    =
    // Built from the end, so the walk below can go forwards.
    let common: int array2d =
        Array2D.zeroCreate (expected.Length + 1) (actual.Length + 1)

    for i in expected.Length - 1 .. -1 .. 0 do
        for j in actual.Length - 1 .. -1 .. 0 do
            common.[i, j] <-
                if expected.[i] = actual.[j] then
                    common.[i + 1, j + 1] + 1
                else
                    max common.[i + 1, j] common.[i, j + 1]

    let rec walk (i: int) (j: int) (lines: DiffLine list) : DiffLine list =
        if i < expected.Length && j < actual.Length && expected.[i] = actual.[j] then
            walk (i + 1) (j + 1) (Same(expectedStart + i, actualStart + j, expected.[i]) :: lines)
        elif
            i < expected.Length
            && (j = actual.Length || common.[i + 1, j] >= common.[i, j + 1])
        then
            walk (i + 1) j (Removed(expectedStart + i, expected.[i]) :: lines)
        elif j < actual.Length then
            walk i (j + 1) (Added(actualStart + j, actual.[j]) :: lines)
        else
            List.rev lines

    walk 0 0 []

/// A patience diff, as WoofWare.Expect draws one: lines that occur exactly once on each side anchor
/// the match, and only the stretches between anchors are diffed line by line. JSON repeats `{`, `},`
/// and `],` everywhere, and a plain diff pairs those up across unrelated messages.
let rec private patienceDiff
    (expected: string array)
    (actual: string array)
    (expectedStart: int)
    (actualStart: int)
    : DiffLine list
    =
    let uniqueIn (lines: string array) : Map<string, int> =
        lines
        |> Array.indexed
        |> Array.groupBy snd
        |> Array.choose (fun (line, occurrences) ->
            match occurrences with
            | [| index, _ |] -> Some(line, index)
            | _ -> None
        )
        |> Map.ofArray

    let uniqueExpected: Map<string, int> = uniqueIn expected
    let uniqueActual: Map<string, int> = uniqueIn actual

    // Unique on both sides, in the order they appear in `expected`.
    let candidates: (int * int) array =
        uniqueExpected
        |> Map.toArray
        |> Array.choose (fun (line, i) -> uniqueActual.TryFind line |> Option.map (fun j -> i, j))
        |> Array.sortBy fst

    // The longest run of candidates that is in order on the `actual` side too. Quadratic, which is
    // nothing at the size of a conversation.
    let anchors: (int * int) list =
        if Array.isEmpty candidates then
            []
        else

        let longest: int array = Array.create candidates.Length 1
        let previous: int array = Array.create candidates.Length -1

        for k in 0 .. candidates.Length - 1 do
            for m in 0 .. k - 1 do
                if snd candidates.[m] < snd candidates.[k] && longest.[m] + 1 > longest.[k] then
                    longest.[k] <- longest.[m] + 1
                    previous.[k] <- m

        let rec chain (k: int) (acc: (int * int) list) : (int * int) list =
            if k < 0 then
                acc
            else
                chain previous.[k] (candidates.[k] :: acc)

        chain (Array.findIndexBack (fun length -> length = Array.max longest) longest) []

    if List.isEmpty anchors then
        commonSubsequenceDiff expected actual expectedStart actualStart
    else

    let rec between (i: int) (j: int) (anchors: (int * int) list) : DiffLine list =
        match anchors with
        | [] -> patienceDiff expected.[i..] actual.[j..] (expectedStart + i) (actualStart + j)
        | (anchorI, anchorJ) :: rest ->

        patienceDiff expected.[i .. anchorI - 1] actual.[j .. anchorJ - 1] (expectedStart + i) (actualStart + j)
        @ [ Same(expectedStart + anchorI, actualStart + anchorJ, expected.[anchorI]) ]
        @ between (anchorI + 1) (anchorJ + 1) rest

    between 0 0 anchors

/// The diff between `expected` and `actual`, numbered by line on each side, with a few unchanged lines
/// around each change and the rest left out.
let private lineDiff (expected: string array) (actual: string array) : string =
    let lines: DiffLine array = patienceDiff expected actual 0 0 |> Array.ofList
    let context: int = 3

    let isChange (line: DiffLine) : bool =
        match line with
        | Same _ -> false
        | Removed _
        | Added _ -> true

    let nearChange (index: int) : bool =
        lines.[max 0 (index - context) .. min (lines.Length - 1) (index + context)]
        |> Array.exists isChange

    let number (index: int) : string = (index + 1).ToString().PadLeft 4

    let render (line: DiffLine) : string =
        match line with
        | Same(i, j, text) -> $"  %s{number i} %s{number j}  %s{text}"
        | Removed(i, text) -> $"- %s{number i}       %s{text}"
        | Added(j, text) -> $"+      %s{number j}  %s{text}"

    lines
    |> Array.mapi (fun index line -> if nearChange index then Some(render line) else None)
    |> Array.fold
        (fun (shown: string list) (line: string option) ->
            match line, shown with
            | Some line, _ -> line :: shown
            | None, "..." :: _ -> shown
            | None, _ -> "..." :: shown
        )
        []
    |> List.rev
    |> String.concat "\n"

/// Compare `actual` with the snapshot at `snapshot`.
///
/// A mismatch fails with a line diff and writes what came out beside the snapshot, as
/// `<name>.received<extension>`, to be looked at and renamed over the snapshot to accept it. With
/// `FANTOMAS_UPDATE_SNAPSHOTS=1` the snapshot is rewritten instead, which is what the
/// `UpdateSnapshots` pipeline of `build.fsx` does.
let verify (snapshot: string) (actual: string) : unit =
    let received: string =
        Path.ChangeExtension(snapshot, ".received" + Path.GetExtension snapshot)

    let actual: string = String.normalizeNewLine actual

    let expected: string option =
        if File.Exists snapshot then
            Some(File.ReadAllText snapshot |> String.normalizeNewLine)
        else
            None

    if expected = Some actual then
        File.Delete received
    elif Environment.GetEnvironmentVariable "FANTOMAS_UPDATE_SNAPSHOTS" = "1" then
        File.WriteAllText(snapshot, actual)
        File.Delete received
    else
        File.WriteAllText(received, actual)

        match expected with
        | None -> Assert.Fail $"There is no snapshot at %s{snapshot} yet. What came out is in %s{received}."
        | Some expected ->

        let diff: string = lineDiff (expected.Split '\n') (actual.Split '\n')
        Assert.Fail $"The output differs from %s{snapshot}. What came out is in %s{received}.\n\n%s{diff}"
