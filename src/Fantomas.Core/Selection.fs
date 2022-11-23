module internal Fantomas.Core.Selection

open FSharp.Compiler.Text
open Fantomas.Core
open Fantomas.Core.FormatConfig
open Fantomas.Core.SyntaxOak
open Fantomas.Core.ISourceTextExtensions

let correctSelection (fileIndex: int) (sourceText: ISourceText) (selection: range) =
    let lines =
        [| selection.StartLine .. selection.EndLine |]
        |> Array.choose (fun lineNumber ->
            let idx = lineNumber - 1

            if idx < 0 then
                None
            else
                let line = sourceText.GetLineString(idx)

                if String.isNotNullOrWhitespace line then
                    Some(lineNumber, line)
                else
                    None)

    match Array.tryHead lines, Array.tryLast lines with
    | Some(startLineNumber, startLine), Some(endLineNumber, endLine) ->
        let startColumn =
            // The selection is on the same line as the code but appears to be inside whitespace
            if startLineNumber = selection.StartLine then
                Seq.takeWhile System.Char.IsWhiteSpace startLine
                |> Seq.length
                |> fun firstCharOnLine -> System.Math.Max(firstCharOnLine, selection.StartColumn)
            else
                // The selection is on a different line than the code, take first non-whitespace character
                Seq.takeWhile System.Char.IsWhiteSpace startLine |> Seq.length

        let endColumn =
            // The selection is on the same line as the code but appears to be inside whitespace
            if endLineNumber = selection.EndLine then
                Seq.rev endLine
                |> Seq.takeWhile System.Char.IsWhiteSpace
                |> Seq.length
                |> fun trimmedEnd -> endLine.Length - trimmedEnd
                |> fun lastCharOnLine -> System.Math.Min(lastCharOnLine, selection.EndColumn)
            else
                // The selection is on a different line than the code, take first non-whitespace character
                Seq.rev endLine
                |> Seq.takeWhile System.Char.IsWhiteSpace
                |> Seq.length
                |> fun trimmedEnd -> endLine.Length - trimmedEnd

        if
            startLineNumber <> selection.StartLine
            || startColumn <> selection.StartColumn
            || endLineNumber <> selection.EndLine
            || endColumn <> selection.EndColumn
        then

            Range.mkFileIndexRange
                fileIndex
                (Position.mkPos startLineNumber startColumn)
                (Position.mkPos endLineNumber endColumn)
        else
            selection
    | _ -> selection

let findNode (selection: range) (node: Node) : Node option =
    let isExactSelection =
        selection.StartLine = node.Range.StartLine
        && selection.StartColumn = node.Range.StartColumn
        && selection.EndLine = node.Range.EndLine
        && selection.EndColumn = node.Range.EndColumn

    if isExactSelection then Some node else None

// let mkAnonSynModuleOrNamespace decl =
//     SynModuleOrNamespace(
//         [],
//         false,
//         SynModuleOrNamespaceKind.AnonModule,
//         [ decl ],
//         PreXmlDoc.Empty,
//         [],
//         None,
//         Range.Zero,
//         { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
//     )
//
// let mkAnonSynModuleOrNamespaceSig decl =
//     SynModuleOrNamespaceSig(
//         [],
//         false,
//         SynModuleOrNamespaceKind.AnonModule,
//         [ decl ],
//         PreXmlDoc.Empty,
//         [],
//         None,
//         Range.Zero,
//         { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
//     )
//
// let mkSynModuleDecl (expr: SynExpr) : SynModuleDecl = SynModuleDecl.Expr(expr, expr.Range)
//
// let mkSynModuleDeclForBinding (binding: SynBinding) : SynModuleDecl =
//     SynModuleDecl.Let(false, [ binding ], binding.FullRange)

[<RequireQualifiedAccess>]
type TreeForSelection =
    /// Format this tree and return the entire result.
    | Standalone of Oak
    /// Format this tree, and extract the first node that matches this type.
    | RequiresExtraction of tree: Oak * nodeType: System.Type
    /// We currently don't support this type of node.
    | Unsupported

/// We can construct a tree using only the node which the user selected.
let mkOakFromModuleDecl (md: ModuleDecl) : TreeForSelection =
    let m = (ModuleDecl.Node md).Range
    TreeForSelection.Standalone(Oak([], [ ModuleOrNamespaceNode(None, [ md ], m) ], m))

/// The selected node by the user cannot be formatted as a standalone expression.
/// We need to format a tree that is an approximation of the selection.
/// This is typically a combination of the selection and some parent construct.
/// For example when formatting a type definition: `let (a: int   list) = []`
/// If the selection is `int    list` we cannot just drop that in an empty file and format it.
/// We can fake a binding (or type alias) and later try and select the formatted type.
let mkExtractableOakFromModule (md: ModuleDecl) (t: System.Type) =
    let m = (ModuleDecl.Node md).Range
    TreeForSelection.RequiresExtraction(Oak([], [ ModuleOrNamespaceNode(None, [ md ], m) ], m), t)

let dummyUnit: Expr =
    UnitNode(SingleTextNode("(", Range.Zero), SingleTextNode(")", Range.Zero), Range.Zero)
    |> Constant.Unit
    |> Expr.Constant

/// Wrap the selected node inside an anonymous module.
/// Keep the original trivia of the ParsedInput so code comments could still be restored.
let mkTreeWithSingleNode (node: Node) : TreeForSelection =
    let m = node.Range

    match node with
    | :? OpenListNode as node -> mkOakFromModuleDecl (ModuleDecl.OpenList node)
    | :? OpenModuleOrNamespaceNode as node ->
        let openMN = Open.ModuleOrNamespace node
        let openList = OpenListNode([ openMN ])
        mkOakFromModuleDecl (ModuleDecl.OpenList openList)
    | :? OpenTargetNode as node ->
        let openT = Open.Target node
        let openList = OpenListNode([ openT ])
        mkOakFromModuleDecl (ModuleDecl.OpenList openList)

    | :? HashDirectiveListNode as node -> mkOakFromModuleDecl (ModuleDecl.HashDirectiveList node)
    | :? ParsedHashDirectiveNode as node ->
        let nodeList = HashDirectiveListNode([ node ])
        mkOakFromModuleDecl (ModuleDecl.HashDirectiveList nodeList)
    | :? ModuleDeclAttributesNode as node -> mkOakFromModuleDecl (ModuleDecl.Attributes node)
    | :? AttributeListNode as node ->
        let attributes = MultipleAttributeListNode([ node ], m)

        let md =
            ModuleDecl.Attributes(ModuleDeclAttributesNode(Some attributes, dummyUnit, m))

        mkExtractableOakFromModule md (node.GetType())

    // ModuleDecl.Expr
    | :? ExprLazyNode as node ->
        let expr = Expr.Lazy node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprSingleNode as node ->
        let expr = Expr.Single node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? UnitNode as node ->
        let expr = Expr.Constant(Constant.Unit node)
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ConstantMeasureNode as node ->
        let expr = Expr.Constant(Constant.Measure node)
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.Null node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprQuoteNode as node ->
        let expr = Expr.Quote node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTypedNode as node ->
        let expr = Expr.Typed node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNewNode as node ->
        let expr = Expr.New node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTupleNode as node ->
        let expr = Expr.Tuple node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprStructTupleNode as node ->
        let expr = Expr.StructTuple node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprArrayOrListNode as node ->
        let expr = Expr.ArrayOrList node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprRecordNode as node ->
        let expr = Expr.Record node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAnonRecordNode as node ->
        let expr = Expr.AnonRecord node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprObjExprNode as node ->
        let expr = Expr.ObjExpr node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprWhileNode as node ->
        let expr = Expr.While node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprForNode as node ->
        let expr = Expr.For node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprForEachNode as node ->
        let expr = Expr.ForEach node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNamedComputationNode as node ->
        let expr = Expr.NamedComputation node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprComputationNode as node ->
        let expr = Expr.Computation node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprCompExprBodyNode as node ->
        let expr = Expr.CompExprBody node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprJoinInNode as node ->
        let expr = Expr.JoinIn node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenLambdaNode as node ->
        let expr = Expr.ParenLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLambdaNode as node ->
        let expr = Expr.Lambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprMatchLambdaNode as node ->
        let expr = Expr.MatchLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprMatchNode as node ->
        let expr = Expr.Match node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTraitCallNode as node ->
        let expr = Expr.TraitCall node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.ParenILEmbedded node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenFunctionNameWithStarNode as node ->
        let expr = Expr.ParenFunctionNameWithStar node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprParenNode as node ->
        let expr = Expr.Paren node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDynamicNode as node ->
        let expr = Expr.Dynamic node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprPrefixAppNode as node ->
        let expr = Expr.PrefixApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprSameInfixAppsNode as node ->
        let expr = Expr.SameInfixApps node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprInfixAppNode as node ->
        let expr = Expr.InfixApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexWithoutDotNode as node ->
        let expr = Expr.IndexWithoutDot node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppDotGetTypeAppNode as node ->
        let expr = Expr.AppDotGetTypeApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetAppDotGetAppParenLambdaNode as node ->
        let expr = Expr.DotGetAppDotGetAppParenLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetAppParenNode as node ->
        let expr = Expr.DotGetAppParen node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetAppWithParenLambdaNode as node ->
        let expr = Expr.DotGetAppWithParenLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetAppNode as node ->
        let expr = Expr.DotGetApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppLongIdentAndSingleParenArgNode as node ->
        let expr = Expr.AppLongIdentAndSingleParenArg node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppSingleParenArgNode as node ->
        let expr = Expr.AppSingleParenArg node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetAppWithLambdaNode as node ->
        let expr = Expr.DotGetAppWithLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppWithLambdaNode as node ->
        let expr = Expr.AppWithLambda node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNestedIndexWithoutDotNode as node ->
        let expr = Expr.NestedIndexWithoutDot node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprEndsWithDualListAppNode as node ->
        let expr = Expr.EndsWithDualListApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprEndsWithSingleListAppNode as node ->
        let expr = Expr.EndsWithSingleListApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprAppNode as node ->
        let expr = Expr.App node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTypeAppNode as node ->
        let expr = Expr.TypeApp node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryWithSingleClauseNode as node ->
        let expr = Expr.TryWithSingleClause node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryWithNode as node ->
        let expr = Expr.TryWith node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTryFinallyNode as node ->
        let expr = Expr.TryFinally node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenNode as node ->
        let expr = Expr.IfThen node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenElseNode as node ->
        let expr = Expr.IfThenElse node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIfThenElifNode as node ->
        let expr = Expr.IfThenElif node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.Ident node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprOptVarNode as node ->
        let expr = Expr.OptVar node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLongIdentSetNode as node ->
        let expr = Expr.LongIdentSet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotIndexedGetNode as node ->
        let expr = Expr.DotIndexedGet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotIndexedSetNode as node ->
        let expr = Expr.DotIndexedSet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprNamedIndexedPropertySetNode as node ->
        let expr = Expr.NamedIndexedPropertySet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotNamedIndexedPropertySetNode as node ->
        let expr = Expr.DotNamedIndexedPropertySet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotGetNode as node ->
        let expr = Expr.DotGet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprDotSetNode as node ->
        let expr = Expr.DotSet node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprSetNode as node ->
        let expr = Expr.Set node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprLibraryOnlyStaticOptimizationNode as node ->
        let expr = Expr.LibraryOnlyStaticOptimization node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprInterpolatedStringExprNode as node ->
        let expr = Expr.InterpolatedStringExpr node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.IndexRangeWildcard node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprTripleNumberIndexRangeNode as node ->
        let expr = Expr.TripleNumberIndexRange node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexRangeNode as node ->
        let expr = Expr.IndexRange node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? ExprIndexFromEndNode as node ->
        let expr = Expr.IndexFromEnd node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | :? SingleTextNode as node ->
        let expr = Expr.Typar node
        mkOakFromModuleDecl (ModuleDecl.DeclExpr expr)
    | _ ->
#if DEBUG
        failwithf "todo for %s" (node.GetType().Name)
#endif
        TreeForSelection.Unsupported

// match fullTree with
// | ParsedInput.ImplFile(ParsedImplFileInput.ParsedImplFileInput(fileName,
//                                                                isScript,
//                                                                qualifiedNameOfFile,
//                                                                scopedPragmas,
//                                                                _hashDirectives,
//                                                                _modules,
//                                                                isLastCompiland,
//                                                                trivia)) ->
//     let insertNode =
//         match astNode with
//         | FSharpASTNode.ModuleDecl synModuleDecl -> synModuleDecl
//         | FSharpASTNode.Expr synExpr -> mkSynModuleDecl synExpr
//         | FSharpASTNode.Binding binding -> mkSynModuleDeclForBinding binding
//         | FSharpASTNode.ModuleSigDecl _
//         | FSharpASTNode.ValSig _ -> failwith "Unexpected signature ast node in implementation file"
//
//     ParsedInput.ImplFile(
//         ParsedImplFileInput.ParsedImplFileInput(
//             fileName,
//             isScript,
//             qualifiedNameOfFile,
//             scopedPragmas,
//             [],
//             [ mkAnonSynModuleOrNamespace insertNode ],
//             isLastCompiland,
//             trivia
//         )
//     )
// | ParsedInput.SigFile(ParsedSigFileInput.ParsedSigFileInput(fileName,
//                                                             qualifiedNameOfFile,
//                                                             scopedPragmas,
//                                                             _hashDirectives,
//                                                             _sigDecls,
//                                                             trivia)) ->
//     let insertNode =
//         match astNode with
//         | FSharpASTNode.ModuleSigDecl decl -> decl
//         | FSharpASTNode.ValSig(SynValSig(range = range) as valSig) -> SynModuleSigDecl.Val(valSig, range)
//         | FSharpASTNode.Expr _
//         | FSharpASTNode.Binding _
//         | FSharpASTNode.ModuleDecl _ -> failwith "Unexpected implementation ast node in implementation file"
//
//     ParsedInput.SigFile(
//         ParsedSigFileInput.ParsedSigFileInput(
//             fileName,
//             qualifiedNameOfFile,
//             scopedPragmas,
//             [],
//             [ mkAnonSynModuleOrNamespaceSig insertNode ],
//             trivia
//         )
//     )

let printTriviaNode (node: Node) : unit =
    let rec visit (level: int) (node: Node) =
        let name = node.GetType().Name
        printfn "%s%s: %A" ("".PadRight(level * 2)) name node.Range
        Array.iter (visit (level + 1)) node.Children

    visit 0 node

// Find the first node that matches the type
let rec findRangeOf (t: System.Type) (root: Node) : range option =
    if root.GetType() = t then
        Some root.Range
    else
        Array.choose (findRangeOf t) root.Children |> Array.tryHead

let formatSelection
    (config: FormatConfig)
    (isSignature: bool)
    (selection: range)
    (sourceText: ISourceText)
    : Async<string * range> =
    async {
        let baseUntypedTree, baseDiagnostics =
            Fantomas.FCS.Parse.parseFile isSignature sourceText []

        let isValid = Validation.noWarningOrErrorDiagnostics baseDiagnostics

        if not isValid then
            raise (FormatException $"Parsing failed with errors: %A{baseDiagnostics}")

        let rootNode = Fangorn.mkOak config (Some sourceText) baseUntypedTree

#if DEBUG
        printTriviaNode rootNode
#endif

        let selection =
            correctSelection (rootNode :> Node).Range.FileIndex sourceText selection

        let treeWithSelection =
            Flowering.findNodeWhereRangeFitsIn rootNode selection
            |> Option.bind (findNode selection)
            |> Option.map mkTreeWithSingleNode

        if treeWithSelection.IsNone then
            raise (FormatException("No suitable AST node was found for the given selection."))

        let tree = treeWithSelection.Value
        let maxLineLength = config.MaxLineLength - selection.StartColumn

        let selectionConfig =
            { config with
                InsertFinalNewline = false
                MaxLineLength = maxLineLength }

        let formattedSelection =
            let context = Context.Context.Create selectionConfig

            match tree with
            | TreeForSelection.Unsupported ->
                raise (FormatException("The current selection is not supported right now."))
            | TreeForSelection.Standalone tree -> CodePrinter2.genFile tree context |> Context.dump true
            | TreeForSelection.RequiresExtraction(tree, t) ->
                let formattedCode = CodePrinter2.genFile tree context |> Context.dump true
                let source = SourceText.ofString formattedCode
                let formattedAST, _ = Fantomas.FCS.Parse.parseFile isSignature source []
                let formattedTree = Fangorn.mkOak selectionConfig (Some source) formattedAST
                let rangeOfSelection = findRangeOf t formattedTree

                match rangeOfSelection with
                | None -> raise (FormatException("No suitable AST node could be extracted from formatted selection."))
                | Some m -> source.GetContentAt m

        // CodeFormatterImpl.formatAST tree (Some sourceText) selectionConfig (Some { Node = node })

        return formattedSelection.TrimEnd([| '\r'; '\n' |]), selection
    }

// TODO: process trivia of selection!
