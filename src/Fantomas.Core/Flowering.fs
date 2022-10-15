module Fantomas.Core.Flowering

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SyntaxOak

let internal collectTriviaFromCodeComments (source: ISourceText) (codeComments: CommentTrivia list) : TriviaNode list =
    codeComments
    |> List.map (function
        | CommentTrivia.BlockComment _ -> failwith "todo, E75069C0-FBC0-4026-9C0D-5BF0773A606F"
        | CommentTrivia.LineComment r ->
            let content = source.GetContentAt r
            let index = r.StartLine - 1
            let line = source.GetLineString index

            let content =
                let trimmedLine = line.TrimStart(' ', ';')

                if index = 0 && trimmedLine.StartsWith("#!") then // shebang
                    CommentOnSingleLine content
                else if trimmedLine.StartsWith("//") then
                    CommentOnSingleLine content
                else
                    failwith "todo, 220DE805-1EDF-426C-9138-91A517DF6726"

            TriviaNode(content, r))

let rec findNodeWhereRangeFitsIn (root: NodeBase) (range: range) : NodeBase option =
    let doesSelectionFitInNode = RangeHelpers.rangeContainsRange root.Range range

    if not doesSelectionFitInNode then
        None
    else
        // The more specific the node fits the selection, the better
        let betterChildNode =
            root.Children
            |> Array.choose (fun childNode -> findNodeWhereRangeFitsIn childNode range)
            |> Array.tryHead

        match betterChildNode with
        | Some betterChild -> Some betterChild
        | None -> Some root

let triviaBeforeOrAfterEntireTree (rootNode: NodeBase) (trivia: TriviaNode) : unit =
    let isBefore = trivia.Range.EndLine < rootNode.Range.StartLine

    if isBefore then
        rootNode.AddBefore(trivia)
    else
        rootNode.AddAfter(trivia)

let simpleTriviaToTriviaInstruction (containerNode: NodeBase) (trivia: TriviaNode) : unit =
    containerNode.Children
    |> Array.tryFind (fun node -> node.Range.StartLine > trivia.Range.StartLine)
    |> Option.map (fun n -> n.AddBefore)
    |> Option.orElseWith (fun () -> Array.tryLast containerNode.Children |> Option.map (fun n -> n.AddAfter))
    |> Option.iter (fun f -> f trivia)

let addToTree (tree: Oak) (trivia: TriviaNode list) =
    for trivia in trivia do
        let smallestNodeThatContainsTrivia = findNodeWhereRangeFitsIn tree trivia.Range

        match smallestNodeThatContainsTrivia with
        | None -> triviaBeforeOrAfterEntireTree tree trivia
        | Some parentNode -> simpleTriviaToTriviaInstruction parentNode trivia

let enrichTree (sourceText: ISourceText) (ast: ParsedInput) (tree: Oak) : Oak =
    let trivia =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (trivia = trivia)) ->
            collectTriviaFromCodeComments sourceText trivia.CodeComments
        | ParsedInput.SigFile (ParsedSigFileInput (trivia = trivia)) ->
            collectTriviaFromCodeComments sourceText trivia.CodeComments

    addToTree tree trivia
    tree
