module internal Fantomas.Core.ASTTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.SyntaxOak

val mkOak: sourceText: ISourceText option -> ast: ParsedInput -> Oak
