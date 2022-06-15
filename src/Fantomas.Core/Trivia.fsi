module internal Fantomas.Core.Trivia

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.TriviaTypes
open Fantomas.Core.FormatConfig

val collectTrivia: FormatConfig -> ISourceText -> ParsedInput -> TriviaForSelection option -> TriviaInstruction list

val printTriviaNode: TriviaNode -> unit

val findNodeWhereRangeFitsIn: TriviaNode -> range -> TriviaNode option
