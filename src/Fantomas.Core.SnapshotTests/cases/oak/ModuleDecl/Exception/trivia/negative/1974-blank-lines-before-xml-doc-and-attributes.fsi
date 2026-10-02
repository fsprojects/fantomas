module internal FSharp.Compiler.ParseHelpers

open FSharp.Compiler.AbstractIL.IL


/// The error raised by the parse_error_rich function, which is called by the parser engine
[<NoEquality; NoComparison>]
exception SyntaxError of obj * range: range
