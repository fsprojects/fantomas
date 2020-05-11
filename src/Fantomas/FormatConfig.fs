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
      SpaceBeforeParameter: bool
      SpaceBeforeLowercaseInvocation: bool
      SpaceBeforeUppercaseInvocation: bool
      SpaceBeforeClassConstructor : bool
      SpaceBeforeMember : bool
      SpaceBeforeColon : bool
      SpaceAfterComma : bool
      SpaceBeforeSemicolon : bool
      SpaceAfterSemicolon : bool
      IndentOnTryWith : bool
      SpaceAroundDelimiter : bool
      MaxIfThenElseShortWidth: Num
      MaxInfixOperatorExpression: Num
      MaxRecordWidth: Num
      MaxArrayOrListWidth: Num
      MaxLetBindingWidth: Num
      MultilineBlockBracketsOnSameColumn : bool
      NewlineBetweenTypeDefinitionAndMembers: bool
      KeepIfThenInSameLine : bool
      /// Prettyprinting based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSpaceNum = 4
          PageWidth = 120
          SemicolonAtEndOfLine = false
          SpaceBeforeParameter = true
          SpaceBeforeLowercaseInvocation = true
          SpaceBeforeUppercaseInvocation = false
          SpaceBeforeClassConstructor = false
          SpaceBeforeMember = false
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceBeforeSemicolon = false
          SpaceAfterSemicolon = true
          IndentOnTryWith = false
          SpaceAroundDelimiter = true
          MaxIfThenElseShortWidth = 40
          MaxInfixOperatorExpression = 50
          MaxRecordWidth = 40
          MaxArrayOrListWidth = 40
          MaxLetBindingWidth = 40
          MultilineBlockBracketsOnSameColumn = false
          KeepIfThenInSameLine = false
          StrictMode = false
          NewlineBetweenTypeDefinitionAndMembers = false }

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
