namespace Fantomas.Core

open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module RangeHelpers =

    /// Checks if Range B is fully contained by Range A
    let rangeContainsRange (a: Range) (b: Range) =
        Position.posGeq b.Start a.Start && Position.posGeq a.End b.End

    let rangeEq = Range.equals

    let isAdjacentTo (r1: Range) (r2: Range) : bool =
        r1.FileName = r2.FileName
        && r1.End.Line = r2.Start.Line
        && r1.EndColumn = r2.StartColumn

    let mkStartRange (size: int) (r: range) : range =
        Range.mkRange r.FileName r.Start (Position.mkPos r.StartLine (r.StartColumn + size))

    let mkStartEndRange (size: int) (r: range) : range * range =
        let startRange = mkStartRange size r

        let endRange =
            Range.mkRange r.FileName (Position.mkPos r.EndLine (r.EndColumn - size)) r.End

        startRange, endRange

    let mergeRanges (ranges: range list) : range option =
        match ranges with
        | [] -> None
        | [ h ] -> Some h
        | all ->
            all
            |> List.sortBy (fun r -> r.StartLine, r.StartColumn)
            |> List.reduce Range.unionRanges
            |> Some

module RangePatterns =
    let (|StartEndRange|) (size: int) (range: range) =
        let o, c = RangeHelpers.mkStartEndRange size range
        o, range, c

    let (|StartRange|) (size: int) (range: range) =
        let startRange =
            Range.mkRange range.FileName range.Start (Position.mkPos range.StartLine (range.StartColumn + size))

        startRange, range
