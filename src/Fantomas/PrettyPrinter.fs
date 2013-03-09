module Fantomas.PrettyPrinter

open System
open System.IO
open System.CodeDom.Compiler
open Fantomas.SourceParser

type Position = ContinueSameLine | BeginNewLine
type Num = int

type FormatConfig = 
    { /// Break into a new line at this column
      PageWidth : Num;
      /// Number of spaces for each identation
      WhiteSpaceNum : Num;
      /// Write parentheses in pattern matching or not
      ParenInPattern : bool;
      /// Length of long identifers (in pattern matching)
      LongIdentLength : Num;
      /// Where to place pipeline operators
      PipelinePos : Position;
      /// Where to place infix operators
      InfixPos : Position;
      /// Has a space before colon?
      SpaceBeforeColon : bool;
      /// Indentation on try with or not?
      IndentOnTryWith : bool;
      /// Number of blank lines between functions and types
      BlankLineNum : Num }
    static member Default = 
        { PageWidth = 120; WhiteSpaceNum = 4; ParenInPattern = true; LongIdentLength = 10;
          PipelinePos = BeginNewLine; InfixPos = ContinueSameLine; SpaceBeforeColon = true;
          IndentOnTryWith = false; BlankLineNum = 1 }

type Context = 
    { Config : FormatConfig; Writer: IndentedTextWriter }
    /// Initialize with a string writer and space as delimiter
    static member Default = { Config = FormatConfig.Default; Writer = new IndentedTextWriter(new StringWriter(), " ") }

let dump (ctx: Context) = ctx.Writer.InnerWriter.ToString()

let incIndent (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent + ctx.Config.WhiteSpaceNum
    ctx

let decIndent (ctx : Context) = 
    ctx.Writer.Indent <- ctx.Writer.Indent - ctx.Config.WhiteSpaceNum
    ctx

/// Function composition operator
let (+>) (ctx : Context -> Context) (f : Context -> Context) x =
    f (ctx x)

let (++>>) (ctx : Context -> Context) (fs : (Context -> Context) seq) =
    Seq.fold (+>) ctx fs

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

let (!!) (str : string) = id ++ str 

/// Call function, but give it context as an argument      
let withCtxt f x =
    (f x) x

let rec genParsedInput = function
    | ImplFile im -> genImpFile im
    | SigFile si -> genSigFile si

and genImpFile = function
    | ParsedImplFileInput(hs, mns) ->
        // Each module is separated by a number of blank lines
        mns |> Seq.map genModuleOrNamespace |> Seq.reduce (+>)

and genSigFile si = failwith "Not implemented yet"

and genModuleOrNamespace = function
    | ModuleOrNamespace(ats, px, ao, li, mds) ->
        mds |> Seq.map genModuleDecl |> Seq.reduce (+>)

and genModuleDecl = function
    | Attributes(a) -> !! "[Attributes]"
    | DoExpr(e) ->  genExpr e
    | Exception(ex) -> genException ex
    | HashDirective(s, ss) -> !! (sprintf "#%s %s" s <| String.concat "." ss)
    | Let(isRec, bs) -> !! "[Let]"
    | ModuleAbbrev(s1, s2) -> !! (sprintf "module %s = %s" s1 s2)
    | NamespaceFragment(m) -> !! "[NamespaceFragment]"
    | NestedModule(ats, px, ao, s, mds) -> 
        id ++ "[Attributes]" ++ "[XmlDocs]" 
        ++ sprintf "module %s%s = " (defaultArg (Option.map(sprintf "%O ") ao) "") s
        +> incIndent
        ++>> Seq.map genModuleDecl mds
    | Open(s) -> !! (sprintf "open %s" s)
    | Types(sts) -> Seq.map genTypeDefn sts |> Seq.reduce (+>)
    | md -> failwithf "Unexpected pattern: %O" md

and genExpr e = id

and genException e = id

and genTypeDefn td = id
        
        




