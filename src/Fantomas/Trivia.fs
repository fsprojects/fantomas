module Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open Fantomas.TokenMatcher

let refDict xs =
    let d = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    xs |> Seq.iter d.Add
    d
    
let collectTrivia content (ast: ParsedInput) =
    let (comments, directives) = filterCommentsAndDirectives content
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
                    comments |> List.partition (fun ((p:pos), _) -> p.Line <= r.StartLine && p.Column <= r.StartCol)
                | None -> [], comments
            List.append
                (commentsBefore |> List.collect snd |> function | [] -> [] | c -> [n.FsAstNode, c])
                (visit comments (n.Childs @ ns))
            | [] -> []
        visit (comments |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.toList) [node]
        |> fun x ->
            refDict x
    | _ -> Seq.empty |> refDict