module internal Fantomas.Trivia

open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open Fantomas
open Fantomas.TriviaTypes

let refDict xs =
    let d = System.Collections.Generic.Dictionary(HashIdentity.Reference)
    xs |> Seq.iter d.Add
    d
    
//type Comment =
//    | LineComment of string
//    | XmlLineComment of string
//    | BlockComment of string

type TriviaIndex = TriviaIndex of int * int

type TriviaNodeType =
    | MainNode
//    | Keyword of string
//    | Token of string
    
type TriviaNode = {
    Type: TriviaNodeType
    CommentsBefore: Comment list
    CommentsAfter: Comment list
}

//let rec parseComments blockAcc comments =
//    match comments with
//    | (p, s:string) :: rest ->
//        let s = s.Trim()
//        let single c = (p, c) :: parseComments None rest
//        blockAcc |> Option.map (fun (p, acc) ->
//            if s.EndsWith "*)" then
//                (p, (BlockComment (acc @ [s.Substring(2, s.Length - 2)] |> String.concat "\n"))) :: parseComments None rest
//            else parseComments (Some (p, acc @ [s])) rest)
//        |> Option.defaultWith (fun () ->
//            if s.StartsWith "///" then XmlLineComment (s.Substring 3) |> single
//            elif s.StartsWith "//" then LineComment (s.Substring 2) |> single
//            elif s.StartsWith "(*" && s.EndsWith "*)" then BlockComment (s.Substring(2, s.Length - 4)) |> single
//            elif s.StartsWith "(*" then parseComments (Some (p, [s.Substring 2])) rest
//            else failwithf "%s is not valid comment" s)
//    | [] -> []

let rec private flattenNodeToList (node: Node) =
    [ yield node
      yield! (node.Childs |> List.map flattenNodeToList |> List.collect id) ]

let private findInChildren fn (node: Node) =
    node.Childs
    |> List.choose fn
    |> List.tryHead

let private findFirstNodeOnLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.StartLine = lineNumber
        | _ -> false
    )
    |> List.sortBy (fun { Range = r } ->
        match r with
        | Some range -> range.StartCol
        | None -> -1
    )
    |> List.tryHead


let private findLastNodeOnLine lineNumber (nodes: Node list) : Node option =
    nodes
    |> List.filter (fun n ->
        match n.Range with
        | Some r -> r.EndLine = lineNumber && List.isEmpty n.Childs
        | _ -> false
    )
    |> List.sortByDescending (fun { Range = r } ->
        match r with
        | Some range -> range.EndCol
        | None -> -1
    )
    |> List.tryHead

let private mapTriviaToTriviaNode nodeList trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment)); Range = range } ->
        // A comment that start at the begin of a line, no other source code is before it on that line
        let nextNodeUnder = findFirstNodeOnLine (range.StartLine + 1) nodeList
        Option.toList nextNodeUnder
        |> List.map (fun n ->
            let t = { Type = MainNode
                      CommentsBefore = [LineCommentOnSingleLine(lineComment)]
                      CommentsAfter = [] }
            (n.FsAstNode, [t])
        )
        
    | { Item = Comment(LineCommentAfterSourceCode(lineComment)); Range = range } ->
        let lastNodeOnLine = findLastNodeOnLine (range.StartLine) nodeList
        Option.toList lastNodeOnLine
        |> List.map (fun n ->
            let t = { Type = MainNode
                      CommentsAfter = [LineCommentAfterSourceCode(lineComment)]
                      CommentsBefore = [] }
            (n.FsAstNode, [t])
        )
        
    | _ -> []

let collectTrivia tokens (ast: ParsedInput) =
//    let (comments, directives, keywords) = filterCommentsAndDirectives content
//    let comments =
//        comments |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Seq.sortBy (fun (p,_) -> p.Line, p.Column)
//        |> Seq.collect (fun (p, cs) -> cs |> Seq.map (fun c -> p, c)) |> Seq.toList
//        |> parseComments None
//        |> List.groupBy fst |> List.map (fun (p,g) -> p, List.map snd g)

    // Extra stuff we need is already capture and has regions
    // Now we only need to figure out what to place in what trivia node.
    let trivias = TokenParser.getTriviaFromTokens tokens
    
    match ast with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hs, mns, _)) ->
        let node = Fantomas.AstTransformer.astToNode (mns |> List.collect (function (SynModuleOrNamespace(ats, px, ao, s, mds, isRecursive, isModule, _)) -> s))
        let nodeList = flattenNodeToList node

        trivias
        |> List.map (mapTriviaToTriviaNode nodeList)
        |> List.collect id
        |> refDict

    | _ -> Seq.empty |> refDict
    
let getMainNode index (ts: TriviaNode list) =
    ts |> List.skip index |> List.tryFind (fun t -> t.Type = TriviaNodeType.MainNode)