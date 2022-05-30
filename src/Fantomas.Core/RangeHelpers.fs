namespace Fantomas.Core

open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module RangeHelpers =

    /// Checks if Range B is fully contained by Range A
    let ``range contains`` (a: Range) (b: Range) = Range.rangeContainsRange a b

    // check if b is after a
    let ``range after`` (a: Range) (b: Range) =
        (a.StartLine, a.StartColumn) < (b.StartLine, b.StartColumn)

    let rangeStartEq (r1: Range) (r2: Range) =
        r1.StartLine = r2.StartLine
        && r1.StartColumn = r2.StartColumn

    let rangeEndEq (r1: Range) (r2: Range) =
        r1.EndLine = r2.EndLine
        && r1.EndColumn = r2.EndColumn

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

    /// Calculate an artificial surface area based on the range.
    let surfaceArea (maxLineLength: int) (range: range) : int =
        // Calculate an artificial surface of positions they range consume.
        // Take the max_line_length as size for a blank line
        // This isn't totally accurate, but will do the trick.
        let linesInBetween =
            match [ range.StartLine + 1 .. range.EndLine - 1 ] with
            | []
            | [ _ ] -> 0
            | lines -> lines.Length * maxLineLength

        (maxLineLength - range.StartColumn)
        + linesInBetween
        + range.EndColumn

module RangePatterns =
    let (|StartEndRange|) (size: int) (range: range) =
        let o, c = RangeHelpers.mkStartEndRange size range
        o, range, c

    let (|StartRange|) (size: int) (range: range) =
        let startRange =
            Range.mkRange range.FileName range.Start (Position.mkPos range.StartLine (range.StartColumn + size))

        startRange, range
