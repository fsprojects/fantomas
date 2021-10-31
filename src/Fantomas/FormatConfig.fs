module Fantomas.FormatConfig

open System

let satSolveMaxStepsMaxSteps = 100

type FormatException(msg: string) =
    inherit Exception(msg)

type Num = int

type MultilineFormatterType =
    | CharacterWidth
    | NumberOfItems

    static member ToConfigString(cfg: MultilineFormatterType) =
        match cfg with
        | MultilineFormatterType.CharacterWidth -> "character_width"
        | MultilineFormatterType.NumberOfItems -> "number_of_items"

    static member OfConfigString(cfgString: string) =
        match cfgString with
        | "character_width" -> Some(box MultilineFormatterType.CharacterWidth)
        | "number_of_items" -> Some(box MultilineFormatterType.NumberOfItems)
        | _ -> None

[<RequireQualifiedAccess>]
type EndOfLineStyle =
    | LF
    | CR
    | CRLF
    member x.NewLineString =
        match x with
        | LF -> "\n"
        | CR -> "\r"
        | CRLF -> "\r\n"

    static member FromEnvironment =
        match Environment.NewLine with
        | "\n" -> LF
        | "\r\n" -> CRLF
        | other -> failwithf "Unknown system newline string found: %s" other

    static member ToConfigString(eol: EndOfLineStyle) =
        match eol with
        | EndOfLineStyle.LF -> "lf"
        | EndOfLineStyle.CR -> "cr"
        | EndOfLineStyle.CRLF -> "crlf"

    static member OfConfigString(eolString: string) =
        match eolString with
        | "lf" -> Some(EndOfLineStyle.LF)
        | "cr" -> failwith "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'"
        | "crlf" -> Some(EndOfLineStyle.CRLF)
        | _ -> None

// NOTE: try to keep this list below in sync with the docs (e.g. Documentation.md)
type FormatConfig =
    { /// Number of spaces for each indentation
      IndentSize: Num
      /// The column where we break to new lines
      MaxLineLength: Num
      SemicolonAtEndOfLine: bool
      SpaceBeforeParameter: bool
      SpaceBeforeLowercaseInvocation: bool
      SpaceBeforeUppercaseInvocation: bool
      SpaceBeforeClassConstructor: bool
      SpaceBeforeMember: bool
      SpaceBeforeColon: bool
      SpaceAfterComma: bool
      SpaceBeforeSemicolon: bool
      SpaceAfterSemicolon: bool
      IndentOnTryWith: bool
      SpaceAroundDelimiter: bool
      MaxIfThenElseShortWidth: Num
      MaxInfixOperatorExpression: Num
      MaxRecordWidth: Num
      MaxRecordNumberOfItems: Num
      RecordMultilineFormatter: MultilineFormatterType
      MaxArrayOrListWidth: Num
      MaxArrayOrListNumberOfItems: Num
      ArrayOrListMultilineFormatter: MultilineFormatterType
      MaxValueBindingWidth: Num
      MaxFunctionBindingWidth: Num
      MaxDotGetExpressionWidth: Num
      MultilineBlockBracketsOnSameColumn: bool
      NewlineBetweenTypeDefinitionAndMembers: bool
      KeepIfThenInSameLine: bool
      MaxElmishWidth: Num
      SingleArgumentWebMode: bool
      AlignFunctionSignatureToIndentation: bool
      AlternativeLongMemberDefinitions: bool
      MultiLineLambdaClosingNewline: bool
      DisableElmishSyntax: bool
      EndOfLine: EndOfLineStyle
      KeepIndentInBranch: bool
      BlankLinesAroundNestedMultilineExpressions: bool
      BarBeforeDiscriminatedUnionDeclaration: bool
      /// Pretty printing based on ASTs only
      StrictMode: bool }

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
          MaxRecordNumberOfItems = 1
          RecordMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxArrayOrListWidth = 40
          MaxArrayOrListNumberOfItems = 1
          ArrayOrListMultilineFormatter = MultilineFormatterType.CharacterWidth
          MaxValueBindingWidth = 80
          MaxFunctionBindingWidth = 40
          MaxDotGetExpressionWidth = 50
          MultilineBlockBracketsOnSameColumn = false
          NewlineBetweenTypeDefinitionAndMembers = false
          KeepIfThenInSameLine = false
          MaxElmishWidth = 40
          SingleArgumentWebMode = false
          AlignFunctionSignatureToIndentation = false
          AlternativeLongMemberDefinitions = false
          MultiLineLambdaClosingNewline = false
          DisableElmishSyntax = false
          EndOfLine = EndOfLineStyle.FromEnvironment
          KeepIndentInBranch = false
          BlankLinesAroundNestedMultilineExpressions = true
          BarBeforeDiscriminatedUnionDeclaration = false
          StrictMode = false }
