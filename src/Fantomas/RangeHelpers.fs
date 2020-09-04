namespace Fantomas

open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
module RangeHelpers =

    /// Checks if range B is fully contained by range A
    let ``range contains`` (a: range) (b: range) =
        (a.Start.Line, a.Start.Column)
        <= (b.Start.Line, b.Start.Column)
        && (a.End.Line, a.End.Column)
           >= (b.End.Line, b.End.Column)

    let ``have same range start`` (a: range) (b: range) =
        a.StartLine = b.StartLine
        && a.StartColumn = b.StartColumn

    // check if b is after a
    let ``range after`` (a: range) (b: range) =
        (a.StartLine, a.StartColumn) < (b.StartLine, b.StartColumn)

    let rangeStartEq (r1: range) (r2: range) =
        r1.StartLine = r2.StartLine
        && r1.StartColumn = r2.StartColumn

    let rangeEndEq (r1: range) (r2: range) =
        r1.EndLine = r2.EndLine
        && r1.EndColumn = r2.EndColumn

    let rangeEq (r1: range) (r2: range) = rangeStartEq r1 r2 && rangeEndEq r1 r2
