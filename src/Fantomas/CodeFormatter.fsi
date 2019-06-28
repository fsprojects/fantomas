namespace Fantomas

open System
open Fantomas.FormatConfig
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open FSharp.Compiler.SourceCodeServices

[<Sealed>]
type CodeFormatter =
    /// Parse a source string using given config
    static member Parse : fileName:string * source:string -> (ParsedInput * string list) array
    /// Parse a source string using given config
    static member ParseAsync : fileName:string * source:string * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<(ParsedInput * string list) array> 
    /// Format an abstract syntax tree using an optional source for looking up literals
    static member FormatAST : ast:ParsedInput * fileName:string * source:string option * config:FormatConfig -> string
    
    /// Infer selection around cursor by looking for a pair of '[' and ']', '{' and '}' or '(' and ')'. 
    static member InferSelectionFromCursorPos : fileName:string * cursorPos:pos * source:string -> range
    
    /// Format around cursor delimited by '[' and ']', '{' and '}' or '(' and ')' using given config; keep other parts unchanged. 
    /// (Only use in testing.)
    static member internal FormatAroundCursorAsync : 
        fileName:string * cursorPos:pos * source:string * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
    
    /// Format a source string using given config
    static member FormatDocument : 
        fileName:string * source:string * config:FormatConfig -> string
    
    /// Format a source string using given config
    static member FormatDocumentAsync : 
        fileName:string * source:string * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
    
    /// Format a part of source string using given config, and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
    static member FormatSelection : 
        fileName:string * selection:range * source:string * config:FormatConfig -> string
    
    /// Format a part of source string using given config, and return the (formatted) selected part only.
    /// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
    static member FormatSelectionAsync : 
        fileName:string * selection:range * source:string * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
   
    /// Format a selected part of source string using given config; keep other parts unchanged. 
    /// (Only use in testing.)
    static member internal FormatSelectionInDocumentAsync : 
        fileName:string * selection:range * source:string * config:FormatConfig * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<string>
     
    /// Check whether an AST consists of parsing errors 
    static member IsValidAST : ast:ParsedInput -> bool
    /// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
    static member IsValidFSharpCode : fileName:string * source:string -> bool
    /// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
    static member IsValidFSharpCodeAsync : fileName:string * source:string * projectOptions:FSharpParsingOptions * checker:FSharpChecker -> Async<bool>
    
    static member MakePos : line:int * col:int -> pos
    static member MakeRange : fileName:string * startLine:int * startCol:int * endLine:int * endCol:int -> range