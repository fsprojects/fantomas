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

    let internal ``has content after after that matches`` (findTrivia: TriviaNode -> bool) (contentAfter: TriviaContent -> bool) (trivia: TriviaNode list) =
        List.tryFind findTrivia trivia
        |> Option.map (fun t -> t.ContentAfter |> List.exists contentAfter)
        |> Option.defaultValue false

    let internal ``has content after that ends with`` (findTrivia: TriviaNode -> bool) (contentAfterEnd: TriviaContent -> bool) (trivia: TriviaNode list) =
        List.tryFind findTrivia trivia
        |> Option.bind (fun t -> t.ContentAfter |> List.tryLast |> Option.map contentAfterEnd)
        |> Option.defaultValue false

    let internal ``is token of type`` tokenName (triviaNode: TriviaNode) =
        match triviaNode.Type with
        | Token({ TokenInfo = ti }) -> ti.TokenName = tokenName
        | _ -> false

    let internal ``keyword tokens inside range`` keywords range (trivia: TriviaNode list) =
        trivia
        |> List.choose(fun t ->
            match t.Type with
            | TriviaNodeType.Token({ TokenInfo = { TokenName = tn } })
                when ( RangeHelpers.``range contains`` range t.Range && List.contains tn keywords) ->
                Some tn
            | _ -> None
        )