[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.FormatConfig
// open Fantomas.Core.TriviaTypes

val getSourceText: source: string -> ISourceText

val formatAST:
    ast: ParsedInput ->
    sourceText: ISourceText option ->
    config: FormatConfig ->
        // selection: TriviaForSelection option ->
        string

val parse: isSignature: bool -> source: ISourceText -> Async<ParsedInput>
// Async<(ParsedInput * DefineCombination)[]>

val formatDocument: config: FormatConfig -> isSignature: bool -> source: ISourceText -> Async<string>
