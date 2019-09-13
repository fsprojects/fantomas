namespace Fantomas

open System
open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member ParseAsync : fileName:string * source:SourceOrigin * parsingOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<(ParsedInput * string list) array>

    /// Format an abstract syntax tree using an optional source for looking up literals
    static member FormatASTAsync : ast:ParsedInput * fileName:string * source:SourceOrigin option  * parsingOptions:FSharpParsingOptions * checker:FSharpChecker  * config:FormatConfig -> Async<string>

    /// Format around cursor delimited by '[' and ']', '{' and '}' or '(' and ')' using given config; keep other parts unchanged. 
    /// (Only use in testing.)
    static member internal FormatAroundCursorAsync : 
        fileName:string * cursorPos:pos * source:SourceOrigin * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>

    /// Format a source string using given config
    static member FormatDocumentAsync : 
        fileName:string * source:SourceOrigin * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
        
/// Format a source string using given config
//    static member FormatDocumentAsync :
//        fileName:string * source:SourceOrigin * config:FormatConfig -> Async<string>
    
    /// Format a part of source string using given config, and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
    static member FormatSelectionAsync : 
        fileName:string * selection:range * source:SourceOrigin * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
   
    /// Format a selected part of source string using given config; keep other parts unchanged. 
    /// (Only use in testing.)
    static member internal FormatSelectionInDocumentAsync : 
        fileName:string * selection:range * source:SourceOrigin * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>

    /// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
    static member IsValidFSharpCodeAsync : fileName:string * source:SourceOrigin * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<bool>

//    static member IsValidFSharpCodeAsync : fileName:string * source:SourceOrigin -> Async<bool>

    static member IsValidASTAsync : ast:ParsedInput -> Async<bool>
    
    static member MakePos : line:int * col:int -> pos
    static member MakeRange : fileName:string * startLine:int * startCol:int * endLine:int * endCol:int -> range
        
    /// Infer selection around cursor by looking for a pair of '[' and ']', '{' and '}' or '(' and ')'. 
    static member InferSelectionFromCursorPos : fileName:string * cursorPos:pos * source:SourceOrigin -> range