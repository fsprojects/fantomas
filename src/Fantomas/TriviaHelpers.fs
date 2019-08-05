namespace Fantomas

open FSharp.Compiler.Range
open Fantomas.TriviaTypes

[<RequireQualifiedAccess>]
module TriviaHelpers =
    let internal findByRange (trivia: TriviaNode list) (range: range) =
        trivia
        |> List.tryFind (fun t -> t.Range = range)

    let internal findFirstContentBeforeByRange (trivia: TriviaNode list) (range: range) =
        findByRange trivia range
        |> Option.bind (fun t -> t.ContentBefore |> List.tryHead)