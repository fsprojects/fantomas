namespace Fantomas.Core

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.SyntaxOak

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync: isSignature: bool * source: string -> Async<(ParsedInput * string list) array>

    /// Format an abstract syntax tree
    static member FormatASTAsync: ast: ParsedInput -> Async<FormatResult>

    /// Format an abstract syntax tree using a given config
    static member FormatASTAsync: ast: ParsedInput * config: FormatConfig -> Async<FormatResult>

    /// Format an abstract syntax tree with the original source for trivia processing
    static member FormatASTAsync: ast: ParsedInput * source: string -> Async<FormatResult>

    /// Format an abstract syntax tree with the original source for trivia processing using a given config
    static member FormatASTAsync: ast: ParsedInput * source: string * config: FormatConfig -> Async<FormatResult>

    /// <summary>
    /// Format a source string using an optional config.
    /// </summary>
    /// <param name="isSignature">Determines whether the F# parser will process the source as signature file.</param>
    /// <param name="source">F# source code</param>
    static member FormatDocumentAsync: isSignature: bool * source: string -> Async<FormatResult>

    /// <summary>
    /// Format a source string using an optional config.
    /// </summary>
    /// <param name="isSignature">Determines whether the F# parser will process the source as signature file.</param>
    /// <param name="source">F# source code</param>
    /// <param name="config">Fantomas configuration</param>
    static member FormatDocumentAsync: isSignature: bool * source: string * config: FormatConfig -> Async<FormatResult>

    /// <summary>
    /// Format a source string using an optional config.
    /// </summary>
    /// <param name="isSignature">Determines whether the F# parser will process the source as signature file.</param>
    /// <param name="source">F# source code</param>
    /// <param name="config">Fantomas configuration</param>
    /// <param name="cursor">The location of a cursor, zero-based.</param>
    static member FormatDocumentAsync:
        isSignature: bool * source: string * config: FormatConfig * cursor: pos -> Async<FormatResult>

    /// Format a part of a source string and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. The closest expression inside the selection will be formatted if possible.
    static member FormatSelectionAsync: isSignature: bool * source: string * selection: range -> Async<string * range>

    /// Format a part of source string using given config, and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. The closest expression inside the selection will be formatted if possible.
    static member FormatSelectionAsync:
        isSignature: bool * source: string * selection: range * config: FormatConfig -> Async<string * range>

    /// Check whether an input string is invalid in F# by attempting to parse the code.
    static member IsValidFSharpCodeAsync: isSignature: bool * source: string -> Async<bool>

    /// Returns the version of Fantomas found in the AssemblyInfo
    static member GetVersion: unit -> string

    /// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
    static member MakeRange: fileName: string * startLine: int * startCol: int * endLine: int * endCol: int -> range

    /// Make a pos from line and column
    static member MakePosition: line: int * column: int -> pos

    /// Parse a source string to SyntaxOak
    static member ParseOakAsync: isSignature: bool * source: string -> Async<(Oak * string list) array>

    /// Format SyntaxOak to string
    static member FormatOakAsync: oak: Oak -> Async<string>

    /// Format SyntaxOak to string using given config
    static member FormatOakAsync: oak: Oak * config: FormatConfig -> Async<string>
