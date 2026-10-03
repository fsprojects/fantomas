namespace Fantomas

open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync:
        fileName: string * source: SourceOrigin * parsingOptions: FSharpParsingOptions * checker: FSharpChecker ->
            Async<(ParsedInput * string list) array>
