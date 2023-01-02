module internal Fantomas.Core.CodePrinter

open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

val genFile: oak: Oak -> (Context -> Context)
