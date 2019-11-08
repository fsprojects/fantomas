namespace Fantomas

open FSharp.Compiler.Range

[<RequireQualifiedAccess>]
module RangeHelpers =

/// Checks if range B is fully contained by range A
let ``range contains`` (a:range) (b:range) =
    (a.Start.Line, a.Start.Column) <= (b.Start.Line, b.Start.Column) &&
    (a.End.Line, a.End.Column) >= (b.End.Line, b.End.Column)

let ``have same range start`` (a:range) (b:range) =
    a.StartLine = b.StartLine && a.StartColumn = b.StartColumn