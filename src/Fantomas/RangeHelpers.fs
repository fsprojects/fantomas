namespace Fantomas

open FSharp.Compiler.Text

[<RequireQualifiedAccess>]
module RangeHelpers =

    /// Checks if Range B is fully contained by Range A
    let ``range contains`` (a: Range) (b: Range) =
        (a.Start.Line, a.Start.Column)
        <= (b.Start.Line, b.Start.Column)
        && (a.End.Line, a.End.Column)
           >= (b.End.Line, b.End.Column)

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
