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
      SpaceBeforeUnitArgumentInUppercaseInvocation : bool
      SpaceBeforeUnitArgumentInLowercaseInvocation : bool
      SpaceBeforeParenthesesInUppercaseInvocation : bool
      SpaceBeforeParenthesesInLowercaseInvocation : bool
      SpaceBeforeUnitParameterInUppercaseFunctionDefinition : bool
      SpaceBeforeUnitParameterInLowercaseFunctionDefinition : bool
      SpaceBeforeParenthesesInUppercaseFunctionDefinition : bool
      SpaceBeforeParenthesesInLowercaseFunctionDefinition : bool
      SpaceBeforeClassConstructor : bool
      SpaceBeforeColon : bool
      SpaceAfterComma : bool
      SpaceAfterSemicolon : bool
      IndentOnTryWith : bool
      /// Reordering and deduplicating open statements
      ReorderOpenDeclaration : bool
      SpaceAroundDelimiter : bool
      KeepNewlineAfter : bool
      MaxIfThenElseShortWidth: Num
      /// Prettyprinting based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSpaceNum = 4
          PageWidth = 120
          SemicolonAtEndOfLine = false
          SpaceBeforeUnitArgumentInUppercaseInvocation = false
          SpaceBeforeUnitArgumentInLowercaseInvocation = false
          SpaceBeforeParenthesesInUppercaseInvocation = false
          SpaceBeforeParenthesesInLowercaseInvocation = true
          SpaceBeforeUnitParameterInUppercaseFunctionDefinition = false
          SpaceBeforeUnitParameterInLowercaseFunctionDefinition = false
          SpaceBeforeParenthesesInUppercaseFunctionDefinition = false
          SpaceBeforeParenthesesInLowercaseFunctionDefinition = true
          SpaceBeforeClassConstructor = false
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceAfterSemicolon = true
          IndentOnTryWith = false
          ReorderOpenDeclaration = false
          SpaceAroundDelimiter = true
          KeepNewlineAfter = false
          MaxIfThenElseShortWidth = 40
          StrictMode = false }

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

