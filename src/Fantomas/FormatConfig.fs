module Fantomas.FormatConfig

open System

let satSolveMaxStepsMaxSteps = 100

type FormatException(msg : string) =
    inherit Exception(msg)

type Num = int

// NOTE: try to keep this list below in sync with the docs (e.g. Documentation.md)
type FormatConfig = 
    { /// Number of spaces for each indentation
      IndentSize : Num
      /// The column where we break to new lines
      MaxLineLength : Num
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
      MaxValueBindingWidth: Num
      MaxFunctionBindingWidth: Num
      MultilineBlockBracketsOnSameColumn : bool
      NewlineBetweenTypeDefinitionAndMembers: bool
      KeepIfThenInSameLine : bool
      MaxElmishWidth: Num
      SingleArgumentWebMode: bool
      AlignFunctionSignatureToIndentation: bool
      AlternativeLongMemberDefinitions: bool
      /// Pretty printing based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSize = 4
          MaxLineLength = 120
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
          MaxValueBindingWidth = 40
          MaxFunctionBindingWidth = 40
          MultilineBlockBracketsOnSameColumn = false
          NewlineBetweenTypeDefinitionAndMembers = false
          KeepIfThenInSameLine = false
          MaxElmishWidth = 40
          SingleArgumentWebMode = false
          AlignFunctionSignatureToIndentation = false
          AlternativeLongMemberDefinitions = false
          StrictMode = false }
