namespace Fantomas.Core

open Fantomas.Core.FormatConfig
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync: isSignature: bool * source: string -> Async<(ParsedInput * string list) array>

    /// Format an abstract syntax tree using an optional source for trivia processing
    static member FormatASTAsync: ast: ParsedInput * source: string option * config: FormatConfig -> Async<string>

    /// Format a source string using given config
    static member FormatDocumentAsync: isSignature: bool * source: string * config: FormatConfig -> Async<string>

    /// Format a part of source string using given config, and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. The closest expression inside the selection will be formatted if possible.
    static member FormatSelectionAsync:
        isSignature: bool * source: string * selection: Range * config: FormatConfig -> Async<string * range>

    /// Check whether an input string is invalid in F# by attempting to parse the code.
    static member IsValidFSharpCodeAsync: isSignature: bool * source: string -> Async<bool>

    /// Returns the version of Fantomas found in the AssemblyInfo
    static member GetVersion: unit -> string
