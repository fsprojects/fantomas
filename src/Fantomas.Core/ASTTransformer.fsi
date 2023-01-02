module internal Fantomas.Core.ASTTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.FormatConfig
open Fantomas.Core.SyntaxOak

val mkOak: config: FormatConfig -> sourceText: ISourceText option -> ast: ParsedInput -> Oak
