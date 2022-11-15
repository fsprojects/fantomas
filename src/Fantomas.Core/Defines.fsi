module internal Fantomas.Core.Defines

open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.SyntaxOak

val getDefineCombination: hashDirectives: ConditionalDirectiveTrivia list -> DefineCombination list
