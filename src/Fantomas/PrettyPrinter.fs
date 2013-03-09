module Fantomas.PrettyPrinter

open System
open Microsoft.FSharp.Compiler.Ast

type Position = ContinueSameLine | BeginNewLine
type Num = int

type FormatConfig = 
    { /// Level of indentation
      IndentLevel : Num;
      /// Break into a new line at this column
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
        { IndentLevel = 0; PageWidth = 120; WhiteSpaceNum = 4; ParenInPattern = true; LongIdentLength = 10;
          PipelinePos = BeginNewLine; InfixPos = ContinueSameLine; SpaceBeforeColon = true;
          IndentOnTryWith = false; BlankLineNum = 1 }
    member x.Inc = { x with IndentLevel = x.IndentLevel + x.WhiteSpaceNum }

type FormatConfig with
    static member (+>)(c : FormatConfig, i: ParsedInput) = 
        match i with
        | ParsedInput.ImplFile im -> c +> im
        | ParsedInput.SigFile si -> c +> si

    static member (+>)(c : FormatConfig, i: ParsedImplFileInput) =
        match i with
        | ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, _, mns, _) ->
            // Each module is separated by a number of blank lines
            mns |> Seq.map ((+>) c) |> String.concat (new String('\n', c.BlankLineNum))

    static member (+>)(c : FormatConfig, i: ParsedSigFileInput) = failwith "Not implemented yet"
    static member (+>)(c : FormatConfig, m : SynModuleOrNamespace) =
        match m with
        | SynModuleOrNamespace.SynModuleOrNamespace(li, _, mds, px, ats, ao, _) ->
            c ++>> mds
    
    static member (+>)(c : FormatConfig, md : SynModuleDecl) = 
        // When the module declaration creates one level of indetation?
        match md with
        | SynModuleDecl.Attributes(a, _) -> c ++>> a
        | SynModuleDecl.DoExpr(sp, se, _) -> c +> se // TODO: do smth with sp
        | SynModuleDecl.Exception(se, _) -> c +> se
        | SynModuleDecl.HashDirective(ph, _) -> c +> ph
        | SynModuleDecl.Let(isRec, bs, _) -> "[Let]"
        | SynModuleDecl.ModuleAbbrev(i, li, _) -> sprintf "module %s = %s" (c +> i) (c ++>> li)
        | SynModuleDecl.NamespaceFragment(m) -> c +> m
        | SynModuleDecl.NestedModule(ci, mds, _, _) -> sprintf "%s\n%s" (c +> ci) (c.Inc ++>> mds)
        | SynModuleDecl.Open(LongIdentWithDots(li, _), _) -> sprintf "open %s" (c ++>> li)
        | SynModuleDecl.Types(sts, _) -> sts |> Seq.map ((+>) c) |> String.concat (new String('\n', c.BlankLineNum))
           
    static member (+>)(c : FormatConfig, px : PreXmlDoc) = "[PreXmlDoc]"
    static member (+>)(c : FormatConfig, at : SynAttribute) = "[SynAttribute]"
    static member (+>)(c : FormatConfig, se : SynExpr) = "[SynExpr]"
    static member (+>)(c : FormatConfig, se : SynExceptionDefn) = "[SynExceptionDefn]"
    static member (+>)(c : FormatConfig, ph : ParsedHashDirective) = "[ParsedHashDirective]"
    static member (+>)(c : FormatConfig, bd : SynBinding) = "[SynBinding]"
    static member (+>)(c : FormatConfig, id : Ident) = id.idText
    static member (+>)(c : FormatConfig, td : SynTypeDefn) = "[SynTypeDefn]"
    static member (+>)(c : FormatConfig, ci : SynComponentInfo) =
        match ci with
        | SynComponentInfo.ComponentInfo(ats, ts, _, li, px, _, ao, _) -> 
            sprintf "%s\n%s\nmodule %s%s = " (c +> px) (c ++>> ats) (defaultArg (Option.map (sprintf "%O ") ao) "")
                (c ++>> li)  

    static member (++>>)(c : FormatConfig, lid : LongIdent) =
        lid |> Seq.map ((+>) c) |> String.concat (new String('.', c.BlankLineNum))
    static member (++>>)(c : FormatConfig, mds : SynModuleDecls) = 
        mds |> Seq.map ((+>) c) |> String.concat (new String('\n', c.BlankLineNum))
    static member (++>>)(c : FormatConfig, ats : SynAttributes) = "[SynAttributes]"
        
        




