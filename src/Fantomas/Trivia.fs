module Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open Fantomas.TokenMatcher

let refDict xs =
    let d = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    xs |> Seq.iter d.Add
    d
    
type Comment = Comment of string

type TriviaIndex = TriviaIndex of int * int

type TriviaNodeType =
    | MainNode
    | Keyword of string
    | Token of string
    
type TriviaNode = {
    Type: TriviaNodeType
    CommentsBefore: Comment list
    CommentsAfter: Comment list
}

let collectTrivia content (ast: ParsedInput) =
    let (comments, directives, keywords) = filterCommentsAndDirectives content
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode (mns |> List.collect (function
            (SynModuleOrNamespace(ats, px, ao, s, mds, isRecursive, isModule, _)) -> s))
        let rec visit comments acc =
            match acc with
            | (n:Fantomas.AstTransformer.Node) :: ns ->
            let (commentsBefore, comments) = 
                match n.Range with
                | Some r ->
                    comments |> List.partition (fun ((p:pos), _) ->
                        p.Line < r.StartLine || (p.Line = r.StartLine && p.Column <= r.StartCol))
                | None -> [], comments
            List.append
                (commentsBefore |> List.collect snd |> function
                    | [] -> []
                    | c -> [n.FsAstNode, [{ Type = MainNode; CommentsBefore = List.map Comment c; CommentsAfter = [] }]])
                (visit comments (n.Childs @ ns))
            | [] -> []
        visit (comments |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.sortBy (fun (p,_) -> p.Line, p.Column) |> Seq.toList) [node]
        |> fun x ->
            refDict x
    | _ -> Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)