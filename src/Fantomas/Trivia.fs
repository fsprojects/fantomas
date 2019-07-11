module internal Fantomas.Trivia

open System
open Fantomas.AstTransformer
open FSharp.Compiler.Ast
open Fantomas
open Fantomas.TriviaTypes
open FSharp.Compiler.Range

let private isMainNodeButNotAnonModule (node: TriviaNode) =
    match node.Type with
    | MainNode(t) when (t <> "SynModuleOrNamespace.AnonModule") -> true
    | _ -> false
    
let isMainNode (node: TriviaNode) =
    match node.Type with
    | MainNode(_) -> true
    | _ -> false
    
let isToken (node: TriviaNode) =
    match node.Type with
    | Token(_) -> true
    | _ -> false

let rec private flattenNodeToList (node: Node) =
    [ yield node
      yield! (node.Childs |> List.map flattenNodeToList |> List.collect id) ]
    
let filterNodes nodes =
    let filterOutNodeTypes =
        set [
            "SynExpr.Sequential" // some Sequential nodes are not visited in CodePrinter
            "SynModuleOrNamespace.DeclaredNamespace" // LongIdent inside Namespace is being processed as children.
            "SynExpr.LetOrUse"
        ]
    nodes |> List.filter (fun (n: Node) -> not (Set.contains n.Type filterOutNodeTypes))

let private findFirstNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range =r  } -> r.StartLine = lineNumber)
    |> List.sortBy (fun { Range = r } -> r.StartColumn)
    |> List.tryHead
    
let private nodesContainsBothAnonModuleAndOpen (nodes: TriviaNode list) =
    let mainNodeIs name t =  t.Type = MainNode(name)
    List.exists (mainNodeIs "SynModuleOrNamespace.AnonModule") nodes &&
    List.exists (mainNodeIs "SynModuleDecl.Open") nodes
    
let private findFirstNodeAfterLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun n -> n.Range.StartLine > lineNumber)
    |> fun filteredNodes ->
        match filteredNodes with
        | moduleAndOpens when (nodesContainsBothAnonModuleAndOpen moduleAndOpens) ->
            moduleAndOpens
            |> List.filter (fun t -> t.Type = MainNode("SynModuleDecl.Open"))
            |> List.sortBy (fun t -> t.Range.StartLine)
            |> List.tryHead
        | _ ->
            filteredNodes
            |> List.sortBy (fun { Range = r } -> r.StartLine, r.StartColumn)
            |> List.tryHead

let private findLastNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range = r } -> r.EndLine = lineNumber)
    |> List.sortByDescending (fun { Range = r } -> r.EndColumn, r.StartColumn)
    |> List.tryHead

let private findLastNode (nodes: TriviaNode list) : TriviaNode option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
        |> List.filter isMainNodeButNotAnonModule
        |> List.maxBy (fun tn -> tn.Range.EndLine)
        |> Some

let private findNodeOnLineAndColumn (nodes: TriviaNode list) line column =
    nodes
    |> List.tryFindBack (fun { Range = range } -> range.StartLine = line && range.StartColumn = column)

let private findMemberDefnMemberNodeOnLine (nodes: TriviaNode list) line =
    nodes
    |> List.filter (fun { Type = t; Range = r } -> match t, r.StartLine = line with | MainNode("SynMemberDefn.Member"), true -> true | _ -> false)
    |> List.tryHead

let private findNodeBeforeLineAndColumn (nodes: TriviaNode list) line column =
    nodes
    |> List.tryFindBack (fun { Range = range } -> range.StartLine <= line && range.StartColumn <= column)

let private findNodeBeforeLineFromStart (nodes: TriviaNode list) line =
    nodes
    |> List.filter (fun { Range = range } -> range.StartLine < line)
    |> List.sortByDescending (fun { Range = range } -> range.StartLine, -range.StartColumn)
    |> List.tryFind (fun { Range = range } -> range.StartLine < line)
    
let private findNodeBeforeLineFromEnd (nodes: TriviaNode list) line =
    nodes
    |> List.tryFindBack (fun { Range = range } -> range.StartLine < line) 

let private findNodeAfterLineAndColumn (nodes: TriviaNode list) line column =
    nodes
    |> List.tryFind (fun { Range = range } -> range.StartLine >= line && range.StartColumn >= column)

let private findConstNodeAfter (nodes: TriviaNode list) (range: range) =
    nodes
    |> List.tryFind (fun { Type = t; Range = r } ->
        match t, range.StartLine = r.StartLine, range.StartColumn + 1 = r.StartColumn with
        | MainNode("SynExpr.Const"), true, true -> true
        | _ -> false
    )

let private mapNodeToTriviaNode (node: Node) =
    node.Range
    |> Option.map (fun range ->
        { Type = MainNode(node.Type)
          ContentAfter = []
          ContentBefore = []
          Range = range }
    )
    
let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNode list) (range: range) =
    triviaNodes
    |> List.filter isMainNodeButNotAnonModule
    |> List.forall (fun tn -> tn.Range.EndLine < range.StartLine)

let private updateTriviaNode lens (triviaNodes: TriviaNode list) triviaNode =
    match triviaNode with
    | Some tNode ->
        // There are situations where the same range can be linked to multiple different AST nodes.
        // F.ex a getter and setter in one line.
        // We want to be sure that one node will be projected by the lens function.
        let index =
            triviaNodes
            |> List.findIndex (fun tn -> tn = tNode)
        
        triviaNodes
        |> List.mapi (fun idx tn -> if idx = index then lens tn else tn)
    | None -> triviaNodes

/// like updateTriviaNode, but returns None when triviaNode is None
let private tryUpdateTriviaNode lens (triviaNodes: TriviaNode list) triviaNode =
    triviaNode |> Option.map (fun tn -> updateTriviaNode lens triviaNodes (Some tn))
    
let private findBindingThatStartsWith (triviaNodes: TriviaNode list) column line =
    triviaNodes
    |> List.tryFind (fun t ->
        match t.Type with
        | MainNode("Binding") when (t.Range.StartColumn = column && t.Range.StartLine = line) ->
            true
        | MainNode("SynPat.Named") when (t.Range.StartColumn = column && t.Range.StartLine = line) ->
            true
        | _ -> false
    )

let private findParsedHashOnLineAndEndswith triviaNodes startLine endColumn =
    triviaNodes
    |> List.tryFind (fun t ->
        match t.Type with
        | MainNode("ParsedHashDirective") when (t.Range.StartLine = startLine && t.Range.EndColumn >= endColumn) ->
            true
        | _ -> false
    )

let private addTriviaToTriviaNode (triviaNodes: TriviaNode list) trivia =
    match trivia with
    | { Item = Comment(LineCommentOnSingleLine(lineComment) as comment); Range = range } when (commentIsAfterLastTriviaNode triviaNodes range) ->
        // Comment on is on its own line after all Trivia nodes, most likely at the end of a module
        findLastNode triviaNodes
        |> updateTriviaNode (fun tn ->
            match tn.ContentAfter with
            | [] -> // make sure that comment ends up under the printed code
                LineCommentOnSingleLine(sprintf "%s%s" Environment.NewLine lineComment)
                |> Comment
                |> List.singleton
                |> fun ca -> { tn with ContentAfter = ca }
            | _ ->
                { tn with ContentAfter = List.appendItem tn.ContentAfter (Comment(comment)) }
        ) triviaNodes

    | { Item = Comment(LineCommentOnSingleLine(_) as comment); Range = range } ->
        findFirstNodeAfterLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem  tn.ContentBefore (Comment(comment)) }) triviaNodes

    | { Item = Comment(BlockComment(_) as comment); Range = range } ->
        let nodeAfter = findNodeAfterLineAndColumn triviaNodes range.StartLine range.StartColumn
        let nodeBefore = findNodeBeforeLineAndColumn triviaNodes range.StartLine range.StartColumn
        match nodeBefore, nodeAfter with
        | (Some n), _ when n.Range.EndLine = range.StartLine ->
            Some n |> updateTriviaNode (fun tn ->
                { tn with ContentAfter = tn.ContentAfter @ [Comment(comment)] }) triviaNodes
        | _, (Some n) ->
            Some n |> updateTriviaNode (fun tn -> 
                let newline = if tn.Range.StartLine > range.EndLine then [Newline] else []
                { tn with ContentBefore = tn.ContentBefore @ [Comment(comment)] @ newline }) triviaNodes
        | (Some n), _ ->
            Some n |> updateTriviaNode (fun tn ->
                { tn with ContentAfter = tn.ContentAfter @ [Newline] @ [Comment(comment)] }) triviaNodes
        | None, None -> triviaNodes

    | { Item = Comment(LineCommentAfterSourceCode(_) as comment); Range = range } ->
        findLastNodeOnLine  triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> { tn with ContentAfter = List.appendItem tn.ContentAfter (Comment(comment)) }) triviaNodes

    | { Item = Newline; Range = range } ->
        let nodeAfterLine = findFirstNodeAfterLine triviaNodes range.StartLine
        match nodeAfterLine with
        | Some _ ->
            nodeAfterLine
            |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore Newline }) triviaNodes
        | None ->
            // try and find a node above
            findNodeBeforeLineFromStart triviaNodes range.StartLine
            |> updateTriviaNode (fun tn -> { tn with ContentAfter = List.appendItem tn.ContentAfter Newline }) triviaNodes

    | { Item = Keyword({ Content = keyword} as kw); Range = range } when (keyword = "override" || keyword = "default" || keyword = "member") ->
        findMemberDefnMemberNodeOnLine triviaNodes range.StartLine
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (Keyword(kw)) }) triviaNodes

    | { Item = Keyword({ TokenInfo = {TokenName = tn}} as kw); Range = range } when (tn = "QMARK") ->
        findConstNodeAfter triviaNodes range
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (Keyword(kw)) }) triviaNodes

    | { Item = Keyword(keyword); Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> fun nodeOnLineAndColumn ->
            match nodeOnLineAndColumn with
            | Some _ ->
                nodeOnLineAndColumn
                |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (Keyword(keyword)) }) triviaNodes
            | None ->
                findParsedHashOnLineAndEndswith triviaNodes range.StartLine range.EndColumn
                |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (Keyword(keyword)) }) triviaNodes

    | { Item = Directive(dc,_) as directive; Range = range } ->
        match findFirstNodeAfterLine triviaNodes range.StartLine with
        | Some _ as node ->
            updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore directive }) triviaNodes node
        | None ->
            let findNode nodes =
                if range.StartColumn = 0 then
                    findNodeBeforeLineFromStart nodes range.StartLine
                else
                    findNodeBeforeLineFromEnd nodes range.EndLine

            findNode triviaNodes
            |> updateTriviaNode (fun tn ->
                let addNewline =
                    List.tryLast tn.ContentAfter
                    |> Option.map (fun tnl -> match tnl with | Directive(_,_) -> false | _ -> true)
                    |> Option.defaultValue true
                let directive =
                    (dc, addNewline)
                    |> Directive
                { tn with ContentAfter = List.appendItem tn.ContentAfter directive }) triviaNodes

    | { Item = StringContent(_) as siNode; Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore (siNode) }) triviaNodes

    | { Item = Number(_) as number; Range = range  } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore number }) triviaNodes
        
    | { Item = IdentOperatorAsWord(_) as ifw; Range = range } ->
        findBindingThatStartsWith triviaNodes range.StartColumn range.StartLine
        |> updateTriviaNode (fun tn -> { tn with ContentBefore = List.appendItem tn.ContentBefore ifw }) triviaNodes

    | _ ->
        triviaNodes

let private triviaNodeIsNotEmpty triviaNode =
    not(List.isEmpty triviaNode.ContentAfter) || not(List.isEmpty triviaNode.ContentBefore)

(*
    1. Collect TriviaNode from tokens and AST
    2. Collect TriviaContent from tokens
    3. Merge trivias with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia tokens lineCount (ast: ParsedInput) =
    let node =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hds, mns, _)) ->            
            Fantomas.AstTransformer.astToNode hds mns

        | ParsedInput.SigFile (ParsedSigFileInput.ParsedSigFileInput(_, _, _ , _, mns)) ->
            Fantomas.AstTransformer.sigAstToNode mns
            
    let triviaNodesFromAST =
        flattenNodeToList node
        |> filterNodes
        |> List.map mapNodeToTriviaNode
        |> List.choose id
    let triviaNodesFromTokens = TokenParser.getTriviaNodesFromTokens tokens
    let triviaNodes = triviaNodesFromAST @ triviaNodesFromTokens |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)
    
    let trivias = TokenParser.getTriviaFromTokens tokens lineCount

    match trivias with
    | [] -> []
    | _ ->
        List.fold addTriviaToTriviaNode triviaNodes trivias
        |> List.filter (triviaNodeIsNotEmpty) // only keep nodes where something special needs to happen.
