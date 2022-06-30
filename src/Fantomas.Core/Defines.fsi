module internal Fantomas.Core.Defines

open FSharp.Compiler.SyntaxTrivia
open Fantomas.Core.TriviaTypes

val getDefineCombination: hashDirectives: ConditionalDirectiveTrivia list -> DefineCombination list
