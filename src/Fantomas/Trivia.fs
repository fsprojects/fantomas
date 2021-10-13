module internal Fantomas.Trivia

open FSharp.Compiler.SourceCodeServices
open Fantomas
open Fantomas.SourceParser
open Fantomas.AstTransformer
open Fantomas.TriviaTypes
open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree

let isMainNode (node: TriviaNode) =
    match node.Type with
    | MainNode _ -> true
    | _ -> false

let inline isMainNodeFor nodeType (node: TriviaNodeAssigner) =
    match node.Type with
    | MainNode t when (t = nodeType) -> true
    | _ -> false

let isToken (node: TriviaNode) =
    match node.Type with
    | Token _ -> true
    | _ -> false

let private findFirstNodeOnLine (nodes: TriviaNode list) lineNumber : TriviaNode option =
    nodes
    |> List.filter (fun { Range = r } -> r.StartLine = lineNumber)
    |> List.tryHead

let inline private mainNodeIs name (t: TriviaNodeAssigner) =
    match t.Type with
    | MainNode mn -> mn = name
    | _ -> false

// the member keyword is not part of an AST node range
// so it is not an ideal candidate node to have trivia content
let inline private isNotMemberKeyword (node: TriviaNodeAssigner) =
    match node.Type with
    | Token (MEMBER, _) -> false
    | _ -> true

let private findFirstNodeAfterLine (nodes: TriviaNodeAssigner list) (lineNumber: int) : TriviaNodeAssigner option =
    nodes
    |> List.tryFind
        (fun tn ->
            tn.Range.StartLine > lineNumber
            && isNotMemberKeyword tn)

let private findLastNodeOnLine (nodes: TriviaNodeAssigner list) lineNumber : TriviaNodeAssigner option =
    nodes
    |> List.filter (fun tn -> tn.Range.EndLine = lineNumber)
    |> List.sortByDescending (fun tn -> tn.Range.EndColumn, tn.Range.StartColumn)
    |> fun candidates ->
        match candidates with
        | app :: ident :: _ when
            (app.Range.End = ident.Range.End
             && isMainNodeFor SynExpr_App app
             && isMainNodeFor SynExpr_Ident ident)
            ->
            Some ident
        | h :: _ -> Some h
        | [] -> None

let private findLastNode (nodes: TriviaNodeAssigner list) : TriviaNodeAssigner option =
    match nodes with
    | [] -> None
    | nodes ->
        nodes
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
            | MainNode SynMemberDefn_Member, true
            | MainNode SynMemberSig_Member, true
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
                    | MainNode mn when (mn = SynExpr_Ident) ->
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

let private findConstNumberNodeOnLineAndColumn (nodes: TriviaNodeAssigner list) (constantRange: Range) =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type with
            | MainNode SynConst_Byte
            | MainNode SynConst_SByte
            | MainNode SynConst_Int16
            | MainNode SynConst_Int32
            | MainNode SynConst_Int64
            | MainNode SynConst_UInt16
            | MainNode SynConst_UInt16s
            | MainNode SynConst_UInt32
            | MainNode SynConst_UInt64
            | MainNode SynConst_Double
            | MainNode SynConst_Single
            | MainNode SynConst_Decimal
            | MainNode SynConst_IntPtr
            | MainNode SynConst_UIntPtr
            | MainNode SynConst_UserNum ->
                constantRange.StartLine = tn.Range.StartLine
                && constantRange.StartColumn = tn.Range.StartColumn
            | MainNode EnumCase_ ->
                tn.Range.EndLine = constantRange.EndLine
                && tn.Range.EndColumn = constantRange.EndColumn
            | _ -> false)

let private findNodeForKeywordString (nodes: TriviaNodeAssigner list) (range: Range) =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type with
            | MainNode SynConst_String ->
                tn.Range.StartLine = range.StartLine
                && tn.Range.StartColumn = range.StartColumn
            | MainNode ParsedHashDirective_ ->
                tn.Range.StartLine = range.StartLine
                && tn.Range.EndColumn >= range.EndColumn
            | _ -> false)

let private findSynConstStringNodeAfter (nodes: TriviaNodeAssigner list) (range: Range) =
    nodes
    |> List.tryFind
        (fun tn ->
            match tn.Type, range.StartLine = tn.Range.StartLine, range.StartColumn + 1 = tn.Range.StartColumn with
            | MainNode SynConst_String, true, true -> true
            | _ -> false)

let private commentIsAfterLastTriviaNode (triviaNodes: TriviaNodeAssigner list) (range: Range) =
    let hasNoNodesAfterRange =
        triviaNodes
        |> Seq.exists (fun tn -> tn.Range.EndLine > range.StartLine)
        |> not

    hasNoNodesAfterRange

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
            | MainNode SynPat_Named
            | MainNode SynPat_LongIdent
            | MainNode SynExpr_Ident ->
                t.Range.StartColumn = column
                && t.Range.StartLine = line
            | _ -> false)

let private findParsedHashOnLineAndEndswith (triviaNodes: TriviaNodeAssigner list) startLine endColumn =
    triviaNodes
    |> List.tryFind
        (fun t ->
            match t.Type with
            | MainNode ParsedHashDirective_ ->
                t.Range.StartLine = startLine
                && t.Range.EndColumn >= endColumn
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
            | Some linesBetween ->
                linesBetween + tn.Range.EndLine >= line
                && line > tn.Range.EndLine
            | _ -> false)

let private findASTNodeOfTypeThatContains (nodes: TriviaNodeAssigner list) typeName range =
    nodes
    |> List.filter
        (fun t ->
            match t.Type with
            | TriviaNodeType.MainNode mnt when (mnt = typeName) -> RangeHelpers.``range contains`` t.Range range
            | _ -> false)
    |> List.tryHead

let private addAllTriviaAsContentAfter (trivia: Trivia list) (singleNode: TriviaNodeAssigner) =
    let contentAfter =
        trivia
        |> List.skipWhile (fun tn -> tn.Item = Newline) // skip leading newlines
        |> List.map (fun tn -> tn.Item)

    { Type = singleNode.Type
      Range = singleNode.Range
      ContentBefore = []
      ContentItself = None
      ContentAfter = contentAfter }
    |> List.singleton

let private addTriviaToTriviaNode
    triviaBetweenAttributeAndParentBinding
    (startOfSourceCode: int)
    (triviaNodes: TriviaNodeAssigner list)
    trivia
    =
    match trivia with
    | { Item = Comment (LineCommentOnSingleLine (comment, _))
        Range = range } ->
        match triviaBetweenAttributeAndParentBinding triviaNodes range.StartLine with
        | Some _ as node ->
            updateTriviaNode
                (fun tn -> tn.ContentAfter.Add(Comment(LineCommentOnSingleLine(comment, range))))
                triviaNodes
                node
        | None ->
            let nodeAfterLine =
                findFirstNodeAfterLine triviaNodes range.StartLine

            match nodeAfterLine with
            | Some _ ->
                nodeAfterLine
                |> updateTriviaNode
                    (fun tn -> tn.ContentBefore.Add(Comment(LineCommentOnSingleLine(comment, range))))
                    triviaNodes
            | None ->
                // try and find a node above
                findNodeBeforeLineFromStart triviaNodes range.StartLine
                |> updateTriviaNode
                    (fun tn -> tn.ContentAfter.Add(Comment(LineCommentOnSingleLine(comment, range))))
                    triviaNodes

    | { Item = Comment (BlockComment (comment, _, _))
        Range = range } ->
        let nodeAfter =
            findNodeAfterLineAndColumn triviaNodes range.StartLine range.StartColumn

        let nodeBefore =
            findNodeBeforeLineAndColumn triviaNodes range.StartLine range.StartColumn

        match nodeBefore, nodeAfter with
        | Some nb, Some na when
            (nb.Range.EndLine < range.StartLine
             && na.Range.StartLine > range.EndLine)
            ->
            Some na
            |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Comment(BlockComment(comment, true, true)))) triviaNodes
        | Some n, _ when n.Range.EndLine = range.StartLine ->
            Some n
            |> updateTriviaNode
                (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, false, false))))
                triviaNodes
        | _, Some n ->
            Some n
            |> updateTriviaNode
                (fun tn ->
                    let newline = tn.Range.StartLine > range.EndLine
                    tn.ContentBefore.Add(Comment(BlockComment(comment, false, newline))))
                triviaNodes
        | Some _, _ when (commentIsAfterLastTriviaNode triviaNodes range) ->
            findLastNode triviaNodes
            |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Comment(BlockComment(comment, true, false)))) triviaNodes
        | Some n, _ ->
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
                findFirstNodeAfterLine triviaNodes range.StartLine

            match nodeAfterLine with
            | Some _ ->
                nodeAfterLine
                |> updateTriviaNode (fun tn -> tn.ContentBefore.Add(Newline)) triviaNodes
            | None ->
                // try and find a node above
                findNodeBeforeLineFromStart triviaNodes range.StartLine
                |> updateTriviaNode (fun tn -> tn.ContentAfter.Add(Newline)) triviaNodes

    | { Item = KeywordString _
        Range = range } ->
        findNodeForKeywordString triviaNodes range
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some trivia.Item) triviaNodes
    | { Item = Keyword ({ Content = keyword } as kw)
        Range = range } when
        (keyword = "override"
         || keyword = "default"
         || keyword = "member"
         || keyword = "abstract")
        ->
        findMemberDefnMemberNodeOnLine triviaNodes range.StartLine
        |> updateTriviaNode
            (fun tn ->
                match tn.Type, tn.ContentItself with
                | TriviaNodeType.Token (MEMBER, _), Some (Keyword ({ Content = existingKeywordContent } as token))
                | MainNode SynMemberSig_Member, Some (Keyword ({ Content = existingKeywordContent } as token)) when
                    existingKeywordContent = "abstract"
                    && keyword = "member"
                    ->
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

    | { Item = Keyword { Content = keyword }
        Range = range } when (keyword = "in") ->
        // find In keyword TriviaNode
        triviaNodes
        |> List.tryFind
            (fun tn ->
                match tn.Type with
                | Token (IN, _) -> RangeHelpers.rangeEq range tn.Range
                | _ -> false)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some trivia.Item) triviaNodes

    | { Item = Keyword { Content = keyword }
        Range = range } when
        (keyword = "if"
         || keyword = "then"
         || keyword = "else"
         || keyword = "elif")
        ->
        findNodeOnLineAndColumn triviaNodes range.StartLine range.StartColumn
        |> Option.orElseWith (fun () -> findASTNodeOfTypeThatContains triviaNodes SynExpr_IfThenElse range)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some trivia.Item) triviaNodes

    | { Item = Keyword keyword
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

    | { Item = Directive dc as directive
        Range = range } ->
        match triviaBetweenAttributeAndParentBinding triviaNodes range.StartLine with
        | Some _ as node -> updateTriviaNode (fun tn -> tn.ContentAfter.Add(directive)) triviaNodes node
        | _ ->
            match findFirstNodeAfterLine triviaNodes range.StartLine with
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
        findConstNumberNodeOnLineAndColumn triviaNodes range
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
                    | MainNode SynExpr_Ident
                    | MainNode SynPat_Named
                    | MainNode SynPat_LongIdent
                    | MainNode Ident_
                    | MainNode SynConst_String -> true
                    | _ -> false

                isIdent
                && (t.Range.StartColumn = range.StartColumn
                    || t.Range.StartColumn = range.StartColumn + 1)
                && t.Range.StartLine = range.StartLine)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some iNode) triviaNodes

    | { Item = EmbeddedIL _ as eil
        Range = range } ->
        triviaNodes
        |> List.tryFind
            (fun t ->
                match t.Type with
                | MainNode SynExpr_LibraryOnlyILAssembly -> RangeHelpers.rangeEq t.Range range
                | _ -> false)
        |> updateTriviaNode (fun tn -> tn.ContentItself <- Some eil) triviaNodes
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
    let triviaNodesFromAST =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, hds, mns, _)) -> astToNode hds mns

        | ParsedInput.SigFile (ParsedSigFileInput.ParsedSigFileInput (_, _, _, _, mns)) -> sigAstToNode mns

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

    let trivias =
        TokenParser.getTriviaFromTokens mkRange tokens

    let startOfSourceCode =
        match tokens with
        | h :: _ -> h.LineNumber // Keep track of comments or hash defines before the first AST node
        | _ -> 1

    match trivias with
    | [] -> []
    | _ ->
        match ast, triviaNodes with
        | EmptyFile _, h :: _ -> addAllTriviaAsContentAfter trivias h
        | _ ->
            List.fold
                (addTriviaToTriviaNode triviaBetweenAttributeAndParentBinding startOfSourceCode)
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
