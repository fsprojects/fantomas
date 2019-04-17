module internal Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open FSharp.Compiler.Range
open Fantomas

let refDict xs =
    let d = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    xs |> Seq.iter d.Add
    d
    
type Comment =
    | LineComment of string
    | XmlLineComment of string
    | BlockComment of string

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

let rec parseComments blockAcc comments =
    match comments with
    | (p, s:string) :: rest ->
        let s = s.Trim()
        let single c = (p, c) :: parseComments None rest
        blockAcc |> Option.map (fun (p, acc) ->
            if s.EndsWith "*)" then
                (p, (BlockComment (acc @ [s.Substring(2, s.Length - 2)] |> String.concat "\n"))) :: parseComments None rest
            else parseComments (Some (p, acc @ [s])) rest)
        |> Option.defaultWith (fun () ->
            if s.StartsWith "///" then XmlLineComment (s.Substring 3) |> single
            elif s.StartsWith "//" then LineComment (s.Substring 2) |> single
            elif s.StartsWith "(*" && s.EndsWith "*)" then BlockComment (s.Substring(2, s.Length - 4)) |> single
            elif s.StartsWith "(*" then parseComments (Some (p, [s.Substring 2])) rest
            else failwithf "%s is not valid comment" s)
    | [] -> []

let collectTrivia tokens (ast: ParsedInput) =
//    let (comments, directives, keywords) = filterCommentsAndDirectives content
//    let comments =
//        comments |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.sortBy (fun (p,_) -> p.Line, p.Column)
//        |> Seq.collect (fun (p, cs) -> cs |> Seq.map (fun c -> p, c)) |> Seq.toList
//        |> parseComments None
//        |> List.groupBy fst |> List.map (fun (p,g) -> p, List.map snd g)

    // Extra stuff we need is already capture and has regions
    // Now we only need to figure out what to place in what trivia node.
    let additionalInfo = TokenParser.getAdditionalInfoFromTokens tokens []
    
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode (mns |> List.collect (function
            (SynModuleOrNamespace(ats, px, ao, s, mds, isRecursive, isModule, _)) -> s))
        let rec visit comments acc prevNode =
            let getComments isBefore n comments =
                comments |> List.collect snd |> function
                    | [] -> []
                    | c -> [n.FsAstNode,
                            if isBefore then [{ Type = MainNode; CommentsBefore = c; CommentsAfter = [] }]
                            else [{ Type = MainNode; CommentsBefore = []; CommentsAfter = c }] ]
            match acc with
            | (n:Fantomas.AstTransformer.Node) :: ns ->
            let (commentsBefore, comments) = 
                match n.Range with
                | Some r ->
                    comments |> List.partition (fun ((p:pos), _) ->
                        p.Line < r.StartLine || (p.Line = r.StartLine && p.Column <= r.StartCol))
                | None -> [], comments
            List.append
                (getComments true n commentsBefore)
                (visit comments (n.Childs @ ns) (Some n))
            | [] ->
                prevNode |> Option.map (fun n -> getComments false n comments)
                |> Option.defaultValue []
        visit
            [] //comments
            [node]
            None
        |> fun x ->
            refDict x
    | _ -> Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)