[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.SyntaxOak

val getSourceText: source: string -> ISourceText

val formatAST: ast: ParsedInput -> sourceText: ISourceText option -> config: FormatConfig -> string

val parse: isSignature: bool -> source: ISourceText -> Async<(ParsedInput * DefineCombination)[]>

val formatDocument: config: FormatConfig -> isSignature: bool -> source: ISourceText -> Async<string>
