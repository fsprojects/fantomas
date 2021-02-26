module internal Fantomas.Trivia

open FSharp.Compiler.SourceCodeServices
open Fantomas
open Fantomas.AstTransformer
open Fantomas.TriviaTypes
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree

let inline private isMainNodeButNotAnonModule (node: TriviaNodeAssigner) =
    match node.Type with
    | MainNode (t) when (t <> SynModuleOrNamespace_AnonModule) -> true
    | _ -> false

let inline private isSynAnonModule (node: TriviaNodeAssigner) =
    match node.Type with
    | MainNode (SynModuleOrNamespace_AnonModule)
    | MainNode (SynModuleOrNamespaceSig_AnonModule) -> true
    | _ -> false

let isMainNode (node: TriviaNode) =
    match node.Type with
    | MainNode _ -> true
    | _ -> false

let inline isMainNodeFor nodeType (node: TriviaNodeAssigner) =
    match node.Type with
    | MainNode (t) when (t = nodeType) -> true
    | _ -> false

let isToken (node: TriviaNode) =
    match node.Type with
    | Token _ -> true
    | _ -> false

let rec private flattenNodeToList (node: Node) =
    [ yield node
      yield!
          (node.Childs
           |> List.map flattenNodeToList
           |> List.collect id) ]

let filterNodes nodes =
    let filterOutNodeTypes =
        set [ SynExpr_Sequential // some Sequential nodes are not visited in CodePrinter
              SynModuleOrNamespace_DeclaredNamespace // LongIdent inside Namespace is being processed as children.
              SynModuleOrNamespaceSig_DeclaredNamespace
              SynExpr_LetOrUse
              SynTypeDefnRepr_ObjectModel
              SynTypeDefnRepr_Simple
              TypeDefnSig_
              SynTypeDefnSigRepr_ObjectModel
              SynExpr_Typed
              // SynType_StaticConstant
              SynExpr_CompExpr ]
    // SynExpr_Do ]

    nodes
    |> List.filter (fun (n: Node) -> not (Set.contains n.Type filterOutNodeTypes))

let private findFirstNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range = r } -> r.StartLine = lineNumber)
    |> List.tryHead

let inline private mainNodeIs name (t: TriviaNodeAssigner) =
    match t.Type with
    | MainNode (mn) -> mn = name
    | _ -> false

let private nodesContainsBothAnonModuleAndOpen (nodes: TriviaNodeAssigner list) =
    List.exists (mainNodeIs SynModuleOrNamespace_AnonModule) nodes
    && List.exists (mainNodeIs SynModuleDecl_Open) nodes

// the member keyword is not part of an AST node range
// so it is not an ideal candidate node to have trivia content
let inline private isNotMemberKeyword (node: TriviaNodeAssigner) =
    match node.Type with
    | Token (MEMBER, _) -> false
    | _ -> true

let private findFirstNodeAfterLine
    (nodes: TriviaNodeAssigner list)
    lineNumber
    hasAnonModulesAndOpenStatements
    : TriviaNodeAssigner option =
    nodes
    |> List.tryFind
        (fun tn ->
            if tn.Range.StartLine > lineNumber
               && isNotMemberKeyword tn then
                not (
                    hasAnonModulesAndOpenStatements
                    && mainNodeIs SynModuleOrNamespace_AnonModule tn
                )
            else
                false)

let private findLastNodeOnLine (nodes: TriviaNodeAssigner list) lineNumber : TriviaNodeAssigner option =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine = lineNumber)
    |> List.sortByDescending (fun tn -> tn.Range.EndColumn, tn.Range.StartColumn)
    |> fun candidates ->
        match candidates with
        | app :: ident :: _ when
            (app.Range.End = ident.Range.End
             && isMainNodeFor SynExpr_App app
             && isMainNodeFor SynExpr_Ident ident) -> Some ident
        | h :: _ -> Some h
        | [] -> None

let private findLastNode (nodes: TriviaNodeAssigner list) : TriviaNodeAssigner option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
        |> List.filter isMainNodeButNotAnonModule
        |> List.maxBy (fun tn -> tn.Range.EndLine)
        |> Some

let private findNodeOnLineAndColumn (nodes: TriviaNodeAssigner list) line column =
    nodes
    |> List.tryFindBack
        (fun tn ->
            tn.Range.StartLine = line
            && tn.Range.StartColumn = column)

let private findMemberDefnMemberNodeOnLine (nodes: TriviaNodeAssigner list) line =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type, tn.Range.StartLine = line with
            | MainNode (SynMemberDefn_Member), true
            | MainNode (SynMemberSig_Member), true
            | Token (MEMBER, _), true -> true
            | _ -> false)

let private findNodeBeforeLineAndColumn (nodes: TriviaNodeAssigner list) line column =
    let node =
        nodes
        |> List.tryFindBack
            (fun tn ->
                let range = tn.Range

                range.StartLine <= line
                && range.StartColumn <= column)

    match node with
    | Some tokenNode ->
        match tokenNode.Type with
        | Token (_, t) when (t.TokenInfo.CharClass = FSharpTokenCharKind.Operator) ->
            // pick the matching ident instead of the token
            nodes
            |> List.tryFind
                (fun tn ->
                    match tn.Type with
                    | MainNode (mn) when (mn = SynExpr_Ident) ->
                        tn.Range.StartLine = t.LineNumber
                        && tn.Range.StartColumn = tn.Range.StartColumn
                    | _ -> false)
        | _ -> node
    | _ -> node


let private findNodeBeforeLineFromStart (nodes: TriviaNodeAssigner list) line =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine < line)
    |> List.sortByDescending (fun tn -> tn.Range.EndLine, -tn.Range.StartColumn)
    |> List.tryFind (fun tn -> tn.Range.StartLine < line)

let private findNodeAfterLineAndColumn (nodes: TriviaNodeAssigner list) line column =
    nodes
    |> List.tryFind
        (fun tn ->
            let range = tn.Range

            (range.StartLine > line)
            || (range.StartLine = line
                && range.StartColumn > column))

let private findConstNodeOnLineAndColumn (nodes: TriviaNodeAssigner list) (constantRange: Range) =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type with
            | MainNode (SynExpr_Const)
            | MainNode (SynPat_Const) ->
                constantRange.StartLine = tn.Range.StartLine
                && constantRange.StartColumn = tn.Range.StartColumn
            | MainNode (EnumCase_) ->
                tn.Range.EndLine = constantRange.EndLine
                && tn.Range.EndColumn = constantRange.EndColumn
            | _ -> false)

let private findSynConstStringNodeAfter (nodes: TriviaNodeAssigner list) (range: Range) =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type, range.StartLine = tn.Range.StartLine, range.StartColumn + 1 = tn.Range.StartColumn with
            | MainNode (SynConst_String), true, true -> true
            | _ -> false)

let private mapNodeToTriviaNode (node: Node) =
    node.Range
    |> Option.map
        (fun range ->
            let attributeParent =
                Map.tryFind "linesBetweenParent" node.Properties
                |> Option.bind
                    (fun v ->
                        match v with
                        | :? int as i when (i > 0) -> Some i
                        | _ -> None)

            match attributeParent with
            | Some i -> TriviaNodeAssigner(MainNode(node.Type), range, i)
            | None -> TriviaNodeAssigner(MainNode(node.Type), range))

let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNodeAssigner list) (range: Range) =
    let hasNoNodesAfterRange =
        triviaNodes
        |> Seq.exists
            (fun tn ->
                tn.Range.EndLine > range.StartLine
                && isMainNodeButNotAnonModule tn)
        |> not

    let hasOnlyOneNamedModule =
        triviaNodes
        |> List.tryExactlyOne
        |> Option.map
            (fun mn ->
                match mn.Type with
                | MainNode (SynModuleOrNamespace_NamedModule)
                | MainNode (SynModuleOrNamespaceSig_NamedModule) -> true
                | _ -> false)
        |> Option.defaultValue false

    hasNoNodesAfterRange || hasOnlyOneNamedModule

let private updateTriviaNode (lens: TriviaNodeAssigner -> unit) (triviaNodes: TriviaNodeAssigner list) triviaNode =
    match triviaNode with
    | Some tNode ->
        // There are situations where the same range can be linked to multiple different AST nodes.
        // F.ex a getter and setter in one line.
        // We want to be sure that one node will be projected by the lens function.
        let index =
            triviaNodes
            |> List.findIndex (fun tn -> tn = tNode)

        lens triviaNodes.[index]

        triviaNodes
    //        |> List.mapi (fun idx tn -> if idx = index then lens tn else tn)
    | None -> triviaNodes

let private findNamedPatThatStartsWith (triviaNodes: TriviaNodeAssigner list) column line =
    triviaNodes
    |> List.tryFind
        (fun t ->
            match t.Type with
            | MainNode (SynPat_Named) when
                (t.Range.StartColumn = column
                 && t.Range.StartLine = line) -> true
            | MainNode (SynPat_LongIdent) when
                (t.Range.StartColumn = column
                 && t.Range.StartLine = line) -> true
            | MainNode (SynExpr_Ident) when
                (t.Range.StartColumn = column
                 && t.Range.StartLine = line) -> true
            | _ -> false)

let private findParsedHashOnLineAndEndswith (triviaNodes: TriviaNodeAssigner list) startLine endColumn =
    triviaNodes
    |> List.tryFind
        (fun t ->
            match t.Type with
            | MainNode (ParsedHashDirective_) when
                (t.Range.StartLine = startLine
                 && t.Range.EndColumn >= endColumn) -> true
            | _ -> false)

// Only return the attributeList when the trivia is under it and above the AST node of which the attribute is a child node.
// f.ex.
// [<Foo()>]
// #if BAR
// let meh = ()
// The trivia '#if BAR' should be linked to the [<Foo()>] attribute
//
// The reason for this is that the range of the attribute is not part of the range of the parent binding.
// This can lead to weird results when used in CodePrinter.
let private triviaBetweenAttributeAndParentBinding (triviaNodes: TriviaNodeAssigner list) line =
    triviaNodes
    |> List.tryFind
        (fun tn ->
            match tn.AttributeLinesBetweenParent with
            | Some linesBetween when
                (linesBetween + tn.Range.EndLine >= line
                 && line > tn.Range.EndLine) -> true
            | _ -> false)

let private findASTNodeOfTypeThatContains (nodes: TriviaNodeAssigner list) typeName range =
    nodes
    |> List.filter
        (fun t ->
            match t.Type with
            | TriviaNodeType.MainNode (mnt) when (mnt = typeName) -> RangeHelpers.``range contains`` t.Range range
            | _ -> false)
    |> List.tryHead

let private addAllTriviaToEmptySynModuleOrNamespace (trivias: Trivia list) (singleNode: TriviaNodeAssigner) =
    { Type = singleNode.Type
      Range = singleNode.Range
      ContentBefore = []
      ContentItself = None
      ContentAfter = List.map (fun t -> t.Item) trivias }
    |> List.singleton

let private addTriviaToTriviaNode
    triviaBetweenAttributeAndParentBinding
    hasAnonModulesAndOpenStatements
    (startOfSourceCode: int)
    (triviaNodes: TriviaNodeAssigner list)
    trivia
    =
    match trivia with
    | { Item = Comment (LineCommentOnSingleLine _) as comment
        Range = range } when (commentIsAfterLastTriviaNode triviaNodes range) ->
        // Comment on is on its own line after all Trivia nodes, most likely at the end of a module
        findLastNode triviaNodes
        |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(comment)) triviaNodes

    | { Item = Comment (LineCommentOnSingleLine _ as comment)
        Range = range } ->
        match triviaBetweenAttributeAndParentBinding triviaNodes range.StartLine with
        | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes node
        | None ->
            findFirstNodeAfterLine triviaNodes range.StartLine hasAnonModulesAndOpenStatements
            |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(comment))) triviaNodes

    | { Item = Comment (BlockComment (comment, _, _))
        Range = range } ->
        let nodeAfter =
            findNodeAfterLineAndColumn triviaNodes range.StartLine range.StartColumn

        let nodeBefore =
            findNodeBeforeLineAndColumn triviaNodes range.StartLine range.StartColumn

        match nodeBefore, nodeAfter with
        | (Some n), _ when n.Range.EndLine = range.StartLine ->
            Some n
            |> updateTriviaNode
                (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, false, false))))
                triviaNodes
        | _, (Some n) ->
            Some n
            |> updateTriviaNode
                (fun tn ->
                    let newline = tn.Range.StartLine > range.EndLine
                    tn.ContentBefore.Add(Comment(BlockComment(comment, false, newline))))
                triviaNodes
        | (Some _), _ when (commentIsAfterLastTriviaNode triviaNodes range) ->
            findLastNode triviaNodes
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
        | (Some n), _ ->
            Some n
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
        | None, None -> triviaNodes

    | { Item = Comment (LineCommentAfterSourceCode _ as comment)
        Range = range } ->
        findLastNodeOnLine triviaNodes range.EndLine
        |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(comment))) triviaNodes

    // Newlines are only relevant if they occur after the first line of source code
    | { Item = Newline; Range = range } when (range.StartLine > startOfSourceCode) ->
        match triviaBetweenAttributeAndParentBinding triviaNodes range.StartLine with
        | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Newline)) triviaNodes node
        | _ ->
            let nodeAfterLine =
                findFirstNodeAfterLine triviaNodes range.StartLine hasAnonModulesAndOpenStatements

            match nodeAfterLine with
            | Some _ ->
                nodeAfterLine
                |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Newline)) triviaNodes
            | None ->
                // try and find a node above
                findNodeBeforeLineFromStart triviaNodes range.StartLine
                |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Newline)) triviaNodes

    | { Item = Keyword ({ Content = keyword } as kw)
        Range = range } when
        (keyword = "override"
         || keyword = "default"
         || keyword = "member"
         || keyword = "abstract") ->
        findMemberDefnMemberNodeOnLine triviaNodes range.StartLine
        |> updateTriviaNode
            (fun tn ->
                match tn.Type, tn.ContentItself with
                | TriviaNodeType.Token (MEMBER, _), Some (Keyword ({ Content = existingKeywordContent } as token))
                | MainNode (SynMemberSig_Member), Some (Keyword ({ Content = existingKeywordContent } as token)) when
                    existingKeywordContent = "abstract"
                    && keyword = "member" ->
                    // Combine the two tokens to appear as one
                    let tokenInfo =
                        { token.TokenInfo with
                              RightColumn = kw.TokenInfo.RightColumn }

                    let combinedKeyword =
                        { token with
                              Content = "abstract member"
                              TokenInfo = tokenInfo }

                    tn.ContentItself <- Some(Keyword(combinedKeyword))
                | _ -> tn.ContentItself <- Some(Keyword(kw)))
            triviaNodes

    | { Item = Keyword ({ TokenInfo = { TokenName = tn } } as kw)
        Range = range } when (tn = "QMARK") ->
        findSynConstStringNodeAfter triviaNodes range
        |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Keyword(kw))) triviaNodes

    | { Item = Keyword ({ Content = keyword })
        Range = range } when (keyword = "in") ->
        // find In keyword TriviaNode
        triviaNodes
        |> List.tryFind
            (fun tn ->
                match tn.Type with
                | Token (IN, _) -> RangeHelpers.rangeEq range tn.Range
                | _ -> false)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some trivia.Item) triviaNodes

    | { Item = Keyword ({ Content = keyword })
        Range = range } when
        (keyword = "if"
         || keyword = "then"
         || keyword = "else"
         || keyword = "elif") ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> Option.orElseWith (fun () -> findASTNodeOfTypeThatContains triviaNodes SynExpr_IfThenElse range)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some trivia.Item) triviaNodes

    | { Item = Keyword (keyword)
        Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> fun nodeOnLineAndColumn ->
            match nodeOnLineAndColumn with
            | Some _ ->
                nodeOnLineAndColumn
                |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Keyword(keyword))) triviaNodes
            | None ->
                findParsedHashOnLineAndEndswith triviaNodes range.StartLine range.EndColumn
                |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Keyword(keyword))) triviaNodes

    | { Item = Directive (dc) as directive
        Range = range } ->
        match triviaBetweenAttributeAndParentBinding triviaNodes range.StartLine with
        | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentAfter.Add(directive)) triviaNodes node
        | _ ->
            match findNodeAfterLineAndColumn triviaNodes range.EndLine range.EndColumn with
            | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentBefore.Add(directive)) triviaNodes node
            | None ->
                let findNode nodes =
                    findNodeBeforeLineFromStart nodes range.StartLine

                findNode triviaNodes
                |> updateTriviaNode
                    (fun tn ->
                        let directive = Directive dc
                        tn.ContentAfter.Add(directive))
                    triviaNodes

    | { Item = StringContent _ as siNode
        Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some siNode) triviaNodes

    | { Item = Number _ as number
        Range = range } ->
        findConstNodeOnLineAndColumn triviaNodes range
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some number) triviaNodes

    | { Item = CharContent _ as chNode
        Range = range } ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some chNode) triviaNodes

    | { Item = IdentOperatorAsWord _ as ifw
        Range = range } ->
        findNamedPatThatStartsWith triviaNodes range.StartColumn range.StartLine
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some ifw) triviaNodes

    | { Item = IdentBetweenTicks _ as iNode
        Range = range } ->
        triviaNodes
        |> List.tryFind
            (fun t ->
                let isIdent =
                    match t.Type with
                    | MainNode (SynExpr_Ident)
                    | MainNode (SynPat_Named)
                    | MainNode (SynPat_LongIdent)
                    | MainNode (Ident_)
                    | MainNode (SynConst_String) -> true
                    | _ -> false

                isIdent
                && (t.Range.StartColumn = range.StartColumn
                    || t.Range.StartColumn = range.StartColumn + 1)
                && t.Range.StartLine = range.StartLine)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some iNode) triviaNodes

    | _ -> triviaNodes

let private triviaNodeIsNotEmpty (triviaNode: TriviaNodeAssigner) =
    not (Seq.isEmpty triviaNode.ContentAfter)
    || not (Seq.isEmpty triviaNode.ContentBefore)
    || Option.isSome triviaNode.ContentItself

(*
    1. Collect TriviaNode from tokens and AST
    2. Collect TriviaContent from tokens
    3. Merge trivias with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia (mkRange: MkRange) tokens (ast: ParsedInput) =
    let node =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, hds, mns, _)) -> astToNode hds mns

        | ParsedInput.SigFile (ParsedSigFileInput.ParsedSigFileInput (_, _, _, _, mns)) -> sigAstToNode mns

    let startOfSourceCode =
        match node.Range with
        | Some r -> r.StartLine
        | None -> 1

    let triviaNodesFromAST =
        flattenNodeToList node
        |> filterNodes
        |> List.choose mapNodeToTriviaNode

    let hasAnyAttributesWithLinesBetweenParent =
        List.exists (fun (tn: TriviaNodeAssigner) -> Option.isSome tn.AttributeLinesBetweenParent) triviaNodesFromAST

    let triviaBetweenAttributeAndParentBinding =
        if hasAnyAttributesWithLinesBetweenParent then
            triviaBetweenAttributeAndParentBinding
        else
            (fun _ _ -> None)

    let triviaNodesFromTokens =
        TokenParser.getTriviaNodesFromTokens mkRange tokens

    let triviaNodes =
        triviaNodesFromAST @ triviaNodesFromTokens
        |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

    let hasAnonModulesAndOpenStatements =
        nodesContainsBothAnonModuleAndOpen triviaNodes

    let trivias =
        TokenParser.getTriviaFromTokens mkRange tokens

    match trivias with
    | [] -> []
    | _ ->
        match triviaNodes with
        | [ singleNode ] when (isSynAnonModule singleNode) -> addAllTriviaToEmptySynModuleOrNamespace trivias singleNode
        | _ ->
            List.fold
                (addTriviaToTriviaNode
                    triviaBetweenAttributeAndParentBinding
                    hasAnonModulesAndOpenStatements
                    startOfSourceCode)
                triviaNodes
                trivias
            |> List.choose
                (fun tn ->
                    if triviaNodeIsNotEmpty tn then
                        { Type = tn.Type
                          Range = tn.Range
                          ContentBefore = Seq.toList tn.ContentBefore
                          ContentItself = tn.ContentItself
                          ContentAfter = Seq.toList tn.ContentAfter }
                        |> Some
                    else
                        None)
