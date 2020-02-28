module Fantomas.FormatConfig

open System

let SAT_SOLVE_MAX_STEPS = 100

type FormatException(msg : string) =
    inherit Exception(msg)

type Num = int

type FormatConfig = 
    { /// Number of spaces for each indentation
      IndentSpaceNum : Num
      /// The column where we break to new lines
      PageWidth : Num
      SemicolonAtEndOfLine : bool
      SpaceBeforeArgument : bool
      SpaceBeforeColon : bool
      SpaceAfterComma : bool
      SpaceBeforeSemicolon : bool
      SpaceAfterSemicolon : bool
      IndentOnTryWith : bool
      /// Reordering and deduplicating open statements
      ReorderOpenDeclaration : bool
      SpaceAroundDelimiter : bool
      KeepNewlineAfter : bool
      MaxIfThenElseShortWidth: Num
      AlignBrackets : bool
      /// Prettyprinting based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSpaceNum = 4
          PageWidth = 120
          SemicolonAtEndOfLine = false
          SpaceBeforeArgument = true
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceBeforeSemicolon = false
          SpaceAfterSemicolon = true
          IndentOnTryWith = false
          ReorderOpenDeclaration = false
          SpaceAroundDelimiter = true
          KeepNewlineAfter = false
          MaxIfThenElseShortWidth = 40
          AlignBrackets = false
          StrictMode = false }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, spaceAroundDelimiter) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, 
                         spaceAroundDelimiter, strictMode) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter;
              StrictMode = strictMode }

    static member applyOptions(currentConfig, options) =
        let currentValues = Reflection.getRecordFields currentConfig
        let newValues =
            Array.fold (fun acc (k,v) ->
                Array.map (fun (fn, ev) -> if fn = k then (fn, v) else (fn,ev)) acc
            ) currentValues options
            |> Array.map snd
        let formatConfigType = FormatConfig.Default.GetType()
        Microsoft.FSharp.Reflection.FSharpValue.MakeRecord (formatConfigType, newValues) :?> FormatConfig

type FormatConfigFileParseResult =
    | Success of FormatConfig
    | PartialSuccess of config: FormatConfig * warnings: string list
    | Failure of exn

