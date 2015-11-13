module Fantomas.CodeFormatter

open System
open System.Diagnostics
open System.Collections.Generic
open System.Text.RegularExpressions

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fantomas
open Fantomas.TokenMatcher
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter

let createFormatContextNoFileName isFsiFile sourceCode =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    CodeFormatterImpl.createFormatContext fileName sourceCode

/// Parse a source code string
let parse isFsiFile sourceCode = 
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.parse
    |> Async.RunSynchronously

/// Check whether an AST consists of parsing errors 
let isValidAST ast = 
    CodeFormatterImpl.isValidAST ast

/// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
let isValidFSharpCode isFsiFile sourceCode =
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.isValidFSharpCode
    |> Async.RunSynchronously

/// Format a source string using given config
let formatSourceString isFsiFile sourceCode config =    
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.formatSourceString config
    |> Async.RunSynchronously

/// Format an abstract syntax tree using given config
let formatAST ast sourceCode config = 
    CodeFormatterImpl.formatAST ast sourceCode config
 
/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange startLine startCol endLine endCol = 
    CodeFormatterImpl.makeRange startLine startCol endLine endCol

/// Format a part of source string using given config, and return the (formatted) selected part only.
/// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
let formatSelectionOnly isFsiFile (range : range) (sourceCode : string) config =
    let formatContext = createFormatContextNoFileName isFsiFile sourceCode
    CodeFormatterImpl.formatSelectionOnly range formatContext config
    |> Async.RunSynchronously

 /// Format a selected part of source string using given config; expanded selected ranges to parsable ranges. 
let formatSelectionExpanded isFsiFile (range : range) (sourceCode : string) config =
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.formatSelectionExpanded range config
    |> Async.RunSynchronously

/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString isFsiFile (range : range) (sourceCode : string) config =
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.formatSelectionFromString range config
    |> Async.RunSynchronously

/// Make a position at (line, col) to denote cursor position
let makePos line col = 
    CodeFormatterImpl.makePos line col

/// Format around cursor delimited by '[' and ']', '{' and '}' or '(' and ')' using given config; keep other parts unchanged. 
let formatAroundCursor isFsiFile (cursorPos : pos) (sourceCode : string) config = 
    createFormatContextNoFileName isFsiFile sourceCode
    |> CodeFormatterImpl.formatAroundCursor cursorPos config
    |> Async.RunSynchronously
