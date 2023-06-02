module internal Fantomas.Core.ASTTransformer

open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.Core.SyntaxOak

val mkOak: sourceText: ISourceText option -> ast: ParsedInput -> Oak
