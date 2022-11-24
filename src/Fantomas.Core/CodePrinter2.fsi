module internal Fantomas.Core.CodePrinter2

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

val genFile: oak: Oak -> (Context -> Context)
