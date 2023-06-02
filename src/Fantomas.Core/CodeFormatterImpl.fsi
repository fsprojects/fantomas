[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open Fantomas.FCS.Syntax
open Fantomas.FCS.Text

val getSourceText: source: string -> ISourceText

val formatAST:
    ast: ParsedInput -> sourceText: ISourceText option -> config: FormatConfig -> cursor: pos option -> FormatResult

val parse: isSignature: bool -> source: ISourceText -> Async<(ParsedInput * DefineCombination)[]>

val formatDocument:
    config: FormatConfig -> isSignature: bool -> source: ISourceText -> cursor: pos option -> Async<FormatResult>
