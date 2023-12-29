namespace Fantomas.Core

type internal DefineCombination =
    | DefineCombination of defines: string list

    member Value: string list

    static member Empty: DefineCombination

module internal Defines =
    open FSharp.Compiler.SyntaxTrivia

    val getDefineCombination: hashDirectives: ConditionalDirectiveTrivia list -> DefineCombination list
