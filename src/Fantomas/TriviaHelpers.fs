namespace Fantomas.Core

open FSharp.Compiler.Text
open Fantomas.Core.TriviaTypes

[<RequireQualifiedAccess>]
module internal TriviaHelpers =
    let findInRange (trivia: TriviaNode list) (range: Range) =
        trivia
        |> List.tryFind (fun t -> RangeHelpers.``range contains`` range t.Range)

    let ``has content after after that matches``
        (findTrivia: TriviaNode -> bool)
        (contentAfter: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.map (fun t -> t.ContentAfter |> List.exists contentAfter)
        |> Option.defaultValue false

    let ``has content before that matches``
        (findTrivia: TriviaNode -> bool)
        (contentBefore: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.map (fun t -> t.ContentBefore |> List.exists contentBefore)
        |> Option.defaultValue false

    let ``has content after that ends with``
        (findTrivia: TriviaNode -> bool)
        (contentAfterEnd: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.bind (fun t ->
            t.ContentAfter
            |> List.tryLast
            |> Option.map contentAfterEnd)
        |> Option.defaultValue false

    let ``has content itself that matches`` (predicate: TriviaContent -> bool) range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.exists (fun tn ->
            match RangeHelpers.rangeEq tn.Range range, tn.ContentItself with
            | true, Some t -> predicate t
            | _ -> false)

    let getNodesForTypes types (dict: Map<'t, TriviaNode list>) =
        types
        |> List.map (fun t ->
            if Map.containsKey t dict then
                Map.find t dict
            else
                List.empty)
        |> List.collect id
