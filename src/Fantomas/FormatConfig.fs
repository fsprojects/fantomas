module Fantomas.FormatConfig

open System.IO
open System.CodeDom.Compiler

type Position = ContinueSameLine | BeginNewLine
type Num = int

type FormatConfig = 
    { /// Number of spaces for each identation
      IndentSpaceNum : Num;
      /// Length of long identifers to consider breaking to multiple lines
      LongIdentLength : Num;
      /// Write semicolon at end of line?
      SemicolonAtEndOfLine : bool;
      /// Keep a space before argument?
      SpaceBeforeArgument : bool;
      SpaceBeforeColon : bool;
      SpaceAfterComma : bool;
      SpaceAfterSemicolon : bool;
      /// Indentation on try/with?
      IndentOnTryWith : bool }
    static member Default = 
        { IndentSpaceNum = 4; LongIdentLength = 10;
          SemicolonAtEndOfLine = false; SpaceBeforeArgument = false; SpaceBeforeColon = true;
          SpaceAfterComma = true; SpaceAfterSemicolon = true; IndentOnTryWith = false }

type Context = 
    { Config : FormatConfig; Writer: IndentedTextWriter }
    /// Initialize with a string writer and use space as delimiter
    static member Default = { Config = FormatConfig.Default; Writer = new IndentedTextWriter(new StringWriter(), " ") }

let dump (ctx: Context) = ctx.Writer.InnerWriter.ToString()

// A few utility functions from https://github.com/fsharp/powerpack/blob/master/src/FSharp.Compiler.CodeDom/generator.fs

/// Indent one more level based on configuration
let indent (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent + ctx.Config.IndentSpaceNum
    ctx

/// Unindent one more level based on configuration
let unindent (ctx : Context) = 
    ctx.Writer.Indent <- max 0 (ctx.Writer.Indent - ctx.Config.IndentSpaceNum)
    ctx

/// Increase indent by i spaces
let incrIndent i (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent + i
    ctx

/// Decrease indent by i spaces
let decrIndent i (ctx : Context) = 
    ctx.Writer.Indent <- max 0 (ctx.Writer.Indent - i)
    ctx

/// Apply function f at an absolute indent level
let atIndentLevel level (f : Context -> Context) ctx =
    if level < 0 then
        invalidArg "level" "The indent level cannot be negative."
    let oldLevel = ctx.Writer.Indent
    ctx.Writer.Indent <- level
    let result = f ctx
    ctx.Writer.Indent <- oldLevel
    result

/// Function composition operator
let (+>) (ctx : Context -> Context) (f : Context -> Context) x =
    f (ctx x)

/// Break-line and append specified string
let (++) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    c.Writer.WriteLine()
    c.Writer.Write(str)
    c

/// Append specified string without line-break
let (--) (ctx : Context -> Context) (str : string) x =
    let c = ctx x
    c.Writer.Write(str)
    c

let (!-) (str : string) = id -- str 
let (!+) (str : string) = id ++ str 

/// Call function, but give it context as an argument      
let withCtxt f x =
    (f x) x

/// Print object converted to string
let str (o : 'T) (ctx : Context) =
    ctx.Writer.Write(o.ToString())
    ctx

/// Process collection - keeps context through the whole processing
/// calls 'f' for every element in sequence and 'f'' between every two elements 
/// as a separator. This is a variant that works on typed collections.
let col f' (c : seq<'T>) f (ctx : Context) =
    let mutable tryPick = true in
    let mutable st = ctx
    let e = c.GetEnumerator()   
    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f (e.Current) st
    st

/// If there is a value, apply f and f' accordingly, otherwise, do nothing
let opt f' o f (ctx : Context) =
    match o with
    | Some x -> f' (f x ctx)
    | None -> ctx

/// Similar to col, apply one more function f2 at the end if the input sequence is not empty
let colPost f1 f2 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else f2 (col f1 c f ctx)

/// Similar to col, apply one more function f2 at the beginning if the input sequence is not empty
let colPre f1 f2 (c : seq<'T>) f (ctx : Context) =
    if Seq.isEmpty c then ctx
    else col f1 c f (f2 ctx)

let ifElse b (f1 : Context -> Context) f2 (ctx : Context) =
    if b then f1 ctx else f2 ctx

/// Repeat application of a function n times
let rep n (f : Context -> Context) (ctx : Context) =
    [1..n] |> List.fold (fun c _ -> f c) ctx

let newline = System.Environment.NewLine

let wordAnd = !- " and "  
let wordOf = !- " of "      

// Separator functions        
let sepDot = !- "."
let sepSpace = !- " "      
let sepNln = !+ ""
let sepStar = !- " * "
let sepEq = !- " = "
let sepArrow = !- " -> "
let sepWild = !- "_"
let sepNone = id

let inline sepColon(ctx : Context) = 
    if ctx.Config.SpaceBeforeColon then str " : " ctx else str ": " ctx

let inline sepComma(ctx : Context) = 
    if ctx.Config.SpaceAfterComma then str ", " ctx else str "," ctx

let inline sepSemi(ctx : Context) = 
    if ctx.Config.SpaceAfterSemicolon then str "; " ctx else str ";" ctx

let inline sepSemiNln(ctx : Context) = 
    if ctx.Config.SemicolonAtEndOfLine then str (";" + newline) ctx else str newline ctx

let inline sepBeforeArg(ctx : Context) = 
    if ctx.Config.SpaceBeforeArgument then str " " ctx else str "" ctx

