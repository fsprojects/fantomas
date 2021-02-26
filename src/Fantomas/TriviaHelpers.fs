namespace Fantomas

open FSharp.Compiler.Text
open Fantomas.TriviaTypes

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

    let ``has single line comment before`` (triviaNode: TriviaNode) =
        triviaNode.ContentBefore
        |> List.exists
            (fun tn ->
                match tn with
                | Comment (LineCommentOnSingleLine _) -> true
                | _ -> false)

    let ``has content after that ends with``
        (findTrivia: TriviaNode -> bool)
        (contentAfterEnd: TriviaContent -> bool)
        (trivia: TriviaNode list)
        =
        List.tryFind findTrivia trivia
        |> Option.bind
            (fun t ->
                t.ContentAfter
                |> List.tryLast
                |> Option.map contentAfterEnd)
        |> Option.defaultValue false

    let ``keyword token inside range`` range (trivia: TriviaNode list) =
        trivia
        |> List.choose
            (fun t ->
                match t.Type with
                | TriviaNodeType.Token (_, tok) when (RangeHelpers.``range contains`` range t.Range) -> Some(tok, t)
                | _ -> None)

    let ``keyword token after start column and on same line`` (range: Range) (trivia: TriviaNode list) =
        trivia
        |> List.choose
            (fun t ->
                match t.Type with
                | TriviaNodeType.Token (_, tok) when
                    (range.StartLine = t.Range.StartLine
                     && range.StartColumn < t.Range.StartColumn) -> Some(tok, t)
                | _ -> None)

    let ``has line comment after`` triviaNode =
        triviaNode.ContentAfter
        |> List.filter
            (fun tn ->
                match tn with
                | Comment (LineCommentAfterSourceCode _) -> true
                | _ -> false)
        |> (List.isEmpty >> not)

    let ``has content itself that matches`` (predicate: TriviaContent -> bool) range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.exists
            (fun tn ->
                match RangeHelpers.rangeEq tn.Range range, tn.ContentItself with
                | true, Some (t) -> predicate t
                | _ -> false)

    let ``has content itself that is multiline string`` range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.choose
            (fun tn ->
                match RangeHelpers.rangeEq tn.Range range, tn.ContentItself with
                | true, Some (StringContent (s)) when (String.isMultiline s) -> Some s
                | _ -> None)
        |> List.isNotEmpty

    let private isLineComment =
        function
        | Comment (LineCommentAfterSourceCode _)
        | Comment (LineCommentOnSingleLine _) -> true
        | _ -> false

    let ``has line comments inside`` range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.exists
            (fun tn ->
                RangeHelpers.``range contains`` range tn.Range
                && (List.exists isLineComment tn.ContentBefore
                    || List.exists isLineComment tn.ContentAfter))

    let getNodesForTypes types (dict: Map<'t, TriviaNode list>) =
        types
        |> List.map
            (fun t ->
                if Map.containsKey t dict then
                    Map.find t dict
                else
                    List.empty)
        |> List.collect id

    let hasMultilineString range (triviaNodes: TriviaNode list) =
        triviaNodes
        |> List.exists
            (fun tn ->
                let contentItSelfIsMultilineString () =
                    match tn.ContentItself with
                    | Some (StringContent sc) -> String.isMultiline sc
                    | _ -> false


                RangeHelpers.rangeEq tn.Range range
                && contentItSelfIsMultilineString ())

    let ``get CharContent`` range (nodes: Map<FsAstType, TriviaNode list>) =
        Map.tryFindOrEmptyList SynConst_Char nodes
        |> List.tryFind (fun t -> RangeHelpers.rangeEq t.Range range)
        |> Option.bind
            (fun tv ->
                match tv.ContentItself with
                | Some (CharContent c) -> Some c
                | _ -> None)
