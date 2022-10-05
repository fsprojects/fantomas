module internal Fantomas.Core.Trivia

open System.Collections.Immutable
open Microsoft.FSharp.Core.CompilerServices
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core
open Fantomas.Core.ISourceTextExtensions
open Fantomas.Core.SourceParser
open Fantomas.Core.AstExtensions
open Fantomas.Core.AstTransformer
open Fantomas.Core.TriviaTypes
open Fantomas.Core.FormatConfig

let printTriviaNode (node: TriviaNode) : unit =
    let rec visit (level: int) (node: TriviaNode) =
        printfn "%s%A: %A" ("".PadRight(level * 2)) node.Type node.Range
        Array.iter (visit (level + 1)) node.Children

    visit 0 node

let rec findNodeWhereRangeFitsIn (root: TriviaNode) (range: range) : TriviaNode option =
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

let internal collectTriviaFromDirectives
    (source: ISourceText)
    (directives: ConditionalDirectiveTrivia list)
    (selection: TriviaForSelection option)
    : Trivia list =
    directives
    |> List.map (function
        | ConditionalDirectiveTrivia.If (_, r)
        | ConditionalDirectiveTrivia.Else r
        | ConditionalDirectiveTrivia.EndIf r ->
            let text = (source.GetContentAt r).TrimEnd()

            { Item = TriviaContent.FromDirective text
              Range = r })
    |> fun trivia ->
        match selection with
        | None -> trivia
        | Some { Node = rootNode } ->
            List.filter (fun t -> RangeHelpers.rangeContainsRange rootNode.Range t.Range) trivia

let internal collectTriviaFromCodeComments
    (source: ISourceText)
    (codeComments: CommentTrivia list)
    (selection: TriviaForSelection option)
    : Trivia list =
    codeComments
    |> List.map (function
        | CommentTrivia.BlockComment r ->
            let content = source.GetContentAt r

            let startLine = source.GetLineString(r.StartLine - 1)

            let endLine = source.GetLineString(r.EndLine - 1)

            if
                startLine.TrimStart(' ', ';').StartsWith("(*")
                && endLine.TrimEnd(' ', ';').EndsWith("*)")
            then
                { Item = TriviaContent.FromCommentOnSingleLine content
                  Range = r }
            else
                let content = source.GetContentAt r

                { Item = TriviaContent.FromBlockComment content false false
                  Range = r }

        | CommentTrivia.LineComment r ->
            let content = source.GetContentAt r
            let index = r.StartLine - 1
            let line = source.GetLineString index

            let item =
                let trimmedLine = line.TrimStart(' ', ';')

                if index = 0 && trimmedLine.StartsWith("#!") then // shebang
                    TriviaContent.FromCommentOnSingleLine content
                else if trimmedLine.StartsWith("//") then
                    TriviaContent.FromCommentOnSingleLine content
                else
                    TriviaContent.FromLineCommentAfterSourceCode content

            { Item = item; Range = r })
    |> fun trivia ->
        match selection with
        | None -> trivia
        | Some { Node = rootNode } ->
            List.filter (fun t -> RangeHelpers.rangeContainsRange rootNode.Range t.Range) trivia

let internal collectTriviaFromBlankLines
    (config: FormatConfig)
    (source: ISourceText)
    (triviaNode: TriviaNode)
    (codeComments: CommentTrivia list)
    (codeRange: range)
    : Trivia list =
    if codeRange.StartLine = 0 && codeRange.EndLine = 0 then
        // weird edge cases where there is no source code but only hash defines
        []
    else
        let fileIndex = triviaNode.Range.FileIndex

        let captureLinesIfMultiline (r: range) =
            if r.StartLine = r.EndLine then
                []
            else
                [ r.StartLine .. r.EndLine ]

        let multilineStringsLines =
            let rec visit (node: TriviaNode) (finalContinuation: int list -> int list) =
                let continuations: ((int list -> int list) -> int list) list =
                    Array.toList node.Children |> List.map visit

                let currentLines =
                    match node.Type with
                    | SynConst_String
                    | SynConst_Bytes
                    | SynInterpolatedStringPart_String -> captureLinesIfMultiline node.Range
                    | _ -> []

                let finalContinuation (lines: int list list) : int list =
                    List.collect id (currentLines :: lines) |> finalContinuation

                Continuation.sequence continuations finalContinuation

            visit triviaNode id

        let blockCommentLines =
            codeComments
            |> List.collect (function
                | CommentTrivia.BlockComment r -> captureLinesIfMultiline r
                | CommentTrivia.LineComment _ -> [])

        let ignoreLines =
            Set(
                seq {
                    yield! multilineStringsLines
                    yield! blockCommentLines
                }
            )

        let min = System.Math.Max(0, codeRange.StartLine - 1)

        let max = System.Math.Min(source.Length - 1, codeRange.EndLine - 1)

        (min, [ min..max ])
        ||> List.chooseState (fun count idx ->
            if ignoreLines.Contains(idx + 1) then
                0, None
            else
                let line = source.GetLineString(idx)

                if String.isNotNullOrWhitespace line then
                    0, None
                else
                    let range =
                        let p = Position.mkPos (idx + 1) 0
                        Range.mkFileIndexRange fileIndex p p

                    if count < config.KeepMaxNumberOfBlankLines then
                        (count + 1),
                        Some
                            { Item = TriviaContent.FromNewline
                              Range = range }
                    else
                        count, None)

/// Find the last child node that will be the last node of the parent node.
let rec visitLastChildNode (node: TriviaNode) : TriviaNode =
    match node.Type with
    | SynExpr_IfThenElse
    | SynExpr_App
    | SynExpr_Lambda
    | SynExpr_LetOrUseBang
    | SynBindingKind_Normal
    | SynModuleOrNamespace_AnonModule
    | SynModuleDecl_Expr
    | SynModuleDecl_Let
    | SynModuleDecl_Types
    | SynModuleDecl_NestedModule
    | SynModuleSigDecl_Val
    | SynModuleSigDecl_Types
    | SynModuleSigDecl_NestedModule
    | SynValSig_
    | SynExpr_Match
    | SynExpr_MatchBang
    | SynMatchClause_
    | SynPat_Typed
    | SynPat_Tuple
    | SynType_Tuple
    | SynType_App
    | SynType_Fun
    | SynExpr_Tuple
    | SynUnionCase_
    | SynEnumCase_
    | SynTypeDefn_
    | SynTypeDefnSig_
    | SynMemberDefn_Member -> visitLastChildNode (Array.last node.Children)
    | SynPat_LongIdent when not (Array.isEmpty node.Children) -> visitLastChildNode (Array.last node.Children)
    | _ -> node

let findNodeBeforeLineAndColumn (nodes: TriviaNode seq) line column =
    nodes
    |> Seq.tryFindBack (fun tn ->
        let range = tn.Range

        range.EndLine <= line && range.EndColumn <= column)
    |> Option.map visitLastChildNode

let findNodeAfterLineAndColumn (nodes: TriviaNode seq) line column =
    nodes
    |> Seq.tryFind (fun tn ->
        let range = tn.Range

        (range.StartLine > line)
        || (range.StartLine = line && range.StartColumn > column))

/// The trivia is not a part of the tree
/// Either assign it on top of below the root node
let triviaBeforeOrAfterEntireTree (rootNode: TriviaNode) (triviaGroup: TriviaGroup) : TriviaInstruction =
    let isBefore = triviaGroup.Range.EndLine < rootNode.Range.StartLine

    let trivia =
        match Seq.tryExactlyOne triviaGroup.Trivia with
        | Some ({ Item = TriviaContent.NonCombinableTriviaContent (NonCombinableTriviaContent.BlockComment (commentText,
                                                                                                            _,
                                                                                                            _)) } as trivia) ->
            let item = TriviaContent.FromBlockComment commentText false false
            let trivia = { trivia with Item = item }
            { triviaGroup with Trivia = ImmutableQueue.Create(trivia) }
        | _ -> triviaGroup

    { TriviaGroup = trivia
      Type = rootNode.Type
      Range = rootNode.Range
      AddBefore = isBefore }

let (|BracketTriviaNode|_|) (node: TriviaNode) =
    match node.Type with
    | SynExpr_ArrayOrList_OpeningDelimiter
    | SynExpr_ArrayOrList_ClosingDelimiter -> Some node
    | _ -> None

/// Try to put the trivia on top of the closest node
/// If that didn't work put it after the last node
let simpleTriviaToTriviaInstruction (containerNode: TriviaNode) (triviaGroup: TriviaGroup) : TriviaInstruction option =
    if triviaGroup.StartColumn > 0 then
        // The group contains a code comment start starts at a certain column.
        let nodeAfter =
            Array.tryFind
                (fun (node: TriviaNode) -> node.Range.StartLine > triviaGroup.Range.StartLine)
                containerNode.Children

        match nodeAfter with
        | Some na when na.Range.StartColumn = triviaGroup.StartColumn ->
            // The comment should be assigned to the next node underneath it.
            Some
                { TriviaGroup = triviaGroup
                  Type = na.Type
                  Range = na.Range
                  AddBefore = true }
        | _ ->
            let nodeBefore =
                Array.tryFindBack
                    (fun (node: TriviaNode) -> node.Range.EndLine < triviaGroup.Range.StartLine)
                    containerNode.Children

            let rec findBack startColumn endLine (current: TriviaNode) =
                if current.Range.StartColumn = startColumn && current.Range.EndLine = endLine then
                    Some current
                elif current.Range.EndLine < endLine then
                    // Don't bother searching in the children here.
                    None
                else
                    Seq.rev current.Children |> Seq.tryPick (findBack startColumn endLine)

            nodeBefore
            |> Option.bind (fun nb -> findBack triviaGroup.StartColumn nb.Range.EndLine nb)
            |> Option.map (fun closestNodeBefore ->
                { TriviaGroup = triviaGroup
                  Type = closestNodeBefore.Type
                  Range = closestNodeBefore.Range
                  AddBefore = false })
    else
        let nodeAfter =
            Array.tryFind
                (fun (node: TriviaNode) -> node.Range.StartLine > triviaGroup.Range.StartLine)
                containerNode.Children

        let nodeBefore =
            Array.tryFindBack
                (fun (node: TriviaNode) -> node.Range.EndLine < triviaGroup.Range.StartLine)
                containerNode.Children

        match nodeBefore, nodeAfter with
        | Some nodeBefore, Some (BracketTriviaNode _) when triviaGroup.StartColumn = nodeBefore.Range.StartColumn ->
            Some
                { TriviaGroup = triviaGroup
                  Type = nodeBefore.Type
                  Range = nodeBefore.Range
                  AddBefore = false }
        | _, Some nodeAfter ->
            Some
                { TriviaGroup = triviaGroup
                  Type = nodeAfter.Type
                  Range = nodeAfter.Range
                  AddBefore = true }
        | _ ->
            Array.tryLast containerNode.Children
            |> Option.map (fun node ->
                { TriviaGroup = triviaGroup
                  Type = node.Type
                  Range = node.Range
                  AddBefore = false })

/// Try and find the smallest possible node
let lineCommentAfterSourceCodeToTriviaInstruction
    (containerNode: TriviaNode)
    (triviaGroup: TriviaGroup)
    : TriviaInstruction option =
    let lineNumber = triviaGroup.Range.StartLine

    let result =
        containerNode.Children
        |> Array.filter (fun node -> node.Range.EndLine = lineNumber)
        |> Array.sortByDescending (fun node -> node.Range.StartColumn)
        |> Array.tryHead

    result
    |> Option.map (fun node ->
        let node = visitLastChildNode node

        { TriviaGroup = triviaGroup
          Type = node.Type
          Range = node.Range
          AddBefore = false })

let blockCommentToTriviaInstruction
    (containerNode: TriviaNode)
    (trivia: Trivia)
    (triviaGroup: TriviaGroup)
    : TriviaInstruction option =
    let nodeAfter =
        findNodeAfterLineAndColumn containerNode.Children triviaGroup.Range.StartLine triviaGroup.Range.StartColumn

    let nodeBefore =
        findNodeBeforeLineAndColumn containerNode.Children triviaGroup.Range.StartLine triviaGroup.Range.StartColumn

    let triviaWith newlineBefore newlineAfter =
        match trivia with
        | { Item = TriviaContent.NonCombinableTriviaContent (BlockComment (content, _, _)) } ->
            let item = TriviaContent.FromBlockComment content newlineBefore newlineAfter
            let trivia = { trivia with Item = item }
            { triviaGroup with Trivia = ImmutableQueue.Create(trivia) }
        | _ -> triviaGroup

    match nodeBefore, nodeAfter with
    | Some nb, None when nb.Range.EndLine = triviaGroup.Range.StartLine ->
        Some
            { TriviaGroup = triviaWith false false
              Type = nb.Type
              Range = nb.Range
              AddBefore = false }
    | Some nb, Some na when
        (nb.Range.EndLine < triviaGroup.Range.StartLine
         && na.Range.StartLine > triviaGroup.Range.EndLine)
        ->
        Some
            { TriviaGroup = triviaWith true true
              Type = na.Type
              Range = na.Range
              AddBefore = true }

    | Some nb, _ when nb.Range.EndLine = triviaGroup.Range.StartLine ->
        Some
            { TriviaGroup = triviaWith false false
              Type = nb.Type
              Range = nb.Range
              AddBefore = false }
    | _ -> None

let mapTriviaToTriviaInstruction (rootNode: TriviaNode) (triviaGroup: TriviaGroup) : TriviaInstruction option =
    let smallestNodeThatContainsTrivia =
        findNodeWhereRangeFitsIn rootNode triviaGroup.Range

    match smallestNodeThatContainsTrivia with
    | None -> Some(triviaBeforeOrAfterEntireTree rootNode triviaGroup)
    | Some parentNode ->
        match Seq.tryExactlyOne triviaGroup.Trivia with
        | Some trivia ->
            match trivia.Item with
            | TriviaContent.NonCombinableTriviaContent (NonCombinableTriviaContent.LineCommentAfterSourceCode _) ->
                lineCommentAfterSourceCodeToTriviaInstruction parentNode triviaGroup
            | TriviaContent.NonCombinableTriviaContent (NonCombinableTriviaContent.BlockComment _) ->
                blockCommentToTriviaInstruction parentNode trivia triviaGroup
            | TriviaContent.CombinableTriviaContent _ -> simpleTriviaToTriviaInstruction parentNode triviaGroup
        | None -> simpleTriviaToTriviaInstruction parentNode triviaGroup

let captureTriviaGroupOfState state =
    match state with
    | TriviaGroupState.InitialState -> Choice1Of3()
    | TriviaGroupState.Open (items, range) ->
        assert (range.StartColumn = 0)

        Choice2Of3
            { Trivia = items
              Range = range
              StartColumn = range.StartColumn }
    | TriviaGroupState.Locked (startColumn, leadingTrivia, owner, trailingTrivia, range) ->
        if trailingTrivia.IsEmpty then
            Choice2Of3
                { Trivia = leadingTrivia.Enqueue owner
                  Range = range
                  StartColumn = startColumn }
        else
            let secondGroupRange =
                match Seq.tryExactlyOne trailingTrivia with
                | Some singleTrivia -> singleTrivia.Range
                | None -> trailingTrivia |> Seq.map (fun t -> t.Range) |> Seq.reduce Range.unionRanges

            Choice3Of3(
                { Trivia = leadingTrivia.Enqueue owner
                  Range = range
                  StartColumn = startColumn },
                { Trivia = trailingTrivia
                  Range = secondGroupRange
                  StartColumn = 0 }
            )

let mkTriviaGroupOfSingleItem (trivia: Trivia) =
    { Trivia = ImmutableQueue.Create(trivia)
      Range = trivia.Range
      StartColumn = trivia.Range.StartColumn }

let mkTriviaGroupOfItems (trivia: ImmutableQueue<Trivia>) range =
    { Trivia = trivia
      Range = range
      StartColumn = range.StartColumn }

let rec groupTrivia (state: TriviaGroupState) (groups: TriviaGroup list) (trivia: Trivia list) =
    match trivia with
    | [] ->
        // Split the last group if necessary
        match captureTriviaGroupOfState state with
        | Choice1Of3 _ -> groups
        | Choice2Of3 h -> h :: groups
        | Choice3Of3 (h2, h1) -> h1 :: h2 :: groups
    | trivia :: rest ->
        match state with
        | TriviaGroupState.InitialState ->
            match trivia.Item with
            | NonCombinableTriviaContent _ -> groupTrivia state (mkTriviaGroupOfSingleItem trivia :: groups) rest
            | CombinableTriviaContent (CombinableTriviaContent.FullLine _) ->
                // start open state
                let nextState = TriviaGroupState.Open(ImmutableQueue.Create(trivia), trivia.Range)
                groupTrivia nextState groups rest
            | CombinableTriviaContent (CombinableTriviaContent.Anchored _) ->
                // start locked state
                let nextState =
                    TriviaGroupState.Locked(
                        trivia.Range.StartColumn,
                        ImmutableQueue.Empty,
                        trivia,
                        ImmutableQueue.Empty,
                        trivia.Range
                    )

                groupTrivia nextState groups rest

        | TriviaGroupState.Open (currentOpenItems, range) ->
            let currentTriviaIsOnTheNextLine = range.EndLine + 1 = trivia.Range.StartLine

            match trivia.Item with
            | CombinableTriviaContent (CombinableTriviaContent.FullLine _) when currentTriviaIsOnTheNextLine ->
                // Safe to add to the current open group
                let nextState =
                    TriviaGroupState.Open(currentOpenItems.Enqueue trivia, Range.unionRanges range trivia.Range)

                groupTrivia nextState groups rest
            | CombinableTriviaContent (CombinableTriviaContent.Anchored _) when currentTriviaIsOnTheNextLine ->
                // transform the open to locked
                let nextState =
                    TriviaGroupState.Locked(
                        trivia.Range.StartColumn,
                        currentOpenItems,
                        trivia,
                        ImmutableQueue.Empty,
                        Range.unionRanges range trivia.Range
                    )

                groupTrivia nextState groups rest

            // Either the current trivia is not on the next line or it cannot be combined with a group.
            | NonCombinableTriviaContent _
            | CombinableTriviaContent _ ->
                // Reset the state
                groupTrivia
                    TriviaGroupState.InitialState
                    (mkTriviaGroupOfSingleItem trivia
                     :: mkTriviaGroupOfItems currentOpenItems range :: groups)
                    rest

        | TriviaGroupState.Locked (startColumn, leadingTrivia, owner, trailingTrivia, range) ->
            let currentTriviaIsOnTheNextLine = range.EndLine + 1 = trivia.Range.StartLine
            let currentTriviaHasSameStartColumn = startColumn = trivia.Range.StartColumn

            match trivia.Item with
            | CombinableTriviaContent (CombinableTriviaContent.FullLine _) when currentTriviaIsOnTheNextLine ->
                // Add as trailing trivia. These trailing trivia might later be chopped of.
                let nextState =
                    TriviaGroupState.Locked(
                        startColumn,
                        leadingTrivia,
                        owner,
                        trailingTrivia.Enqueue trivia,
                        Range.unionRanges range trivia.Range
                    )

                groupTrivia nextState groups rest
            | CombinableTriviaContent (CombinableTriviaContent.Anchored _) when
                currentTriviaIsOnTheNextLine && currentTriviaHasSameStartColumn
                ->
                // The comment has the same start column as our owner. We consider it part of the group and make it the new owner.
                let nextState =
                    TriviaGroupState.Locked(
                        startColumn,
                        leadingTrivia.Enqueue owner,
                        trivia,
                        ImmutableQueue.Empty,
                        Range.unionRanges range trivia.Range
                    )

                groupTrivia nextState groups rest
            | NonCombinableTriviaContent _
            | CombinableTriviaContent _ ->
                // Reset the state, however, trailingTrivia should become there owner group.
                let nextGroups =
                    match captureTriviaGroupOfState state with
                    | Choice1Of3 _ -> groups
                    | Choice2Of3 h -> h :: groups
                    | Choice3Of3 (h2, h1) -> h1 :: h2 :: groups

                groupTrivia TriviaGroupState.InitialState nextGroups rest
(*
    1. Collect TriviaNodes from AST
    2. Extract trivia from directives, comments and blank lines
    3. Merge trivia with triviaNodes
    4. genTrivia should use ranges to identify what extra content should be added from what triviaNode
*)
let collectTrivia
    (config: FormatConfig)
    (source: ISourceText)
    (ast: ParsedInput)
    (selection: TriviaForSelection option)
    : TriviaInstruction list =
    let rootNode, directives, codeComments =
        match ast with
        | ParsedInput.ImplFile (ParsedImplFileInput (hds, mns, directives, codeComments)) ->
            let rootNode =
                match selection with
                | None -> astToNode ast.FullRange hds mns
                | Some { Node = rootNode } -> rootNode

            rootNode, directives, codeComments
        | ParsedInput.SigFile (ParsedSigFileInput (_, mns, directives, codeComments)) ->
            let rootNode =
                match selection with
                | None -> sigAstToNode ast.FullRange mns
                | Some { Node = rootNode } -> rootNode

            rootNode, directives, codeComments

    // printTriviaNode rootNode

    let trivia =
        let codeRange =
            match selection with
            | None -> ast.FullRange
            | Some { Node = rootNode } -> rootNode.Range

        let mkTriviaGroup (trivia: Trivia) =
            { Trivia = ImmutableQueue.Create(trivia)
              Range = trivia.Range
              StartColumn = trivia.Range.StartColumn }

        [ yield! collectTriviaFromDirectives source directives selection
          yield! collectTriviaFromCodeComments source codeComments selection
          yield! collectTriviaFromBlankLines config source rootNode codeComments codeRange ]
        |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

        // something recursive instead of folding...
        |> groupTrivia TriviaGroupState.InitialState []

    // |> List.fold
    //     (fun groups trivia ->
    //         match groups with
    //         | [] ->
    //             // Start a new group
    //             [ mkTriviaGroup trivia ]
    //         | currentGroup :: rest ->
    //             match trivia.Item with
    //             | Comment (Comment.BlockComment _ | Comment.LineCommentAfterSourceCode _) ->
    //                 // Start a new group, these type will always have their own group
    //                 mkTriviaGroup trivia :: currentGroup :: rest
    //             | Newline
    //             | Directive _ ->
    //                 if not currentGroup.Trivia.IsEmpty then
    //                     let range =
    //                         Range.mkFileIndexRange
    //                             currentGroup.Range.FileIndex
    //                             currentGroup.Range.Start
    //                             trivia.Range.End
    //
    //                     { currentGroup with
    //                         Trivia = currentGroup.Trivia.Enqueue(trivia)
    //                         Range = range }
    //                     :: rest
    //                 else
    //                     failwith "not implemented yet"
    //             | Comment (CommentOnSingleLine _) ->
    //                 // Check if we can add the trivia to the current group
    //                 if currentGroup.StartColumn = 0 && trivia.Range.StartColumn > 0 then
    //                     // Update the start column of the group
    //                     // The comment will have the most significant start column for the group
    //                     let range =
    //                         Range.mkFileIndexRange
    //                             currentGroup.Range.FileIndex
    //                             currentGroup.Range.Start
    //                             trivia.Range.End
    //
    //                     { currentGroup with
    //                         Trivia = currentGroup.Trivia.Enqueue(trivia)
    //                         Range = range
    //                         StartColumn = trivia.Range.StartColumn }
    //                     :: rest
    //                 elif currentGroup.StartColumn = trivia.Range.StartColumn then
    //                     // Append trivia to group
    //                     let range =
    //                         Range.mkFileIndexRange
    //                             currentGroup.Range.FileIndex
    //                             currentGroup.Range.Start
    //                             trivia.Range.End
    //
    //                     { currentGroup with
    //                         Trivia = currentGroup.Trivia.Enqueue(trivia)
    //                         Range = range }
    //                     :: rest
    //                 else
    //                     // No relationship, start a new group'
    //                     mkTriviaGroup trivia :: currentGroup :: rest)
    //     []

    let mutable instructionCollector = ListCollector<TriviaInstruction>()

    for trivia in trivia do
        let instruction = mapTriviaToTriviaInstruction rootNode trivia

        match instruction with
        | None -> ()
        | Some instruction -> instructionCollector.Add(instruction)

    instructionCollector.Close()
