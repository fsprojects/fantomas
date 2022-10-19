module internal Fantomas.Core.Selection

open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.AstExtensions
open Fantomas.Core.TriviaTypes
open Fantomas.Core.AstTransformer
open Fantomas.Core.Trivia

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
    | Some (startLineNumber, startLine), Some (endLineNumber, endLine) ->
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

let findNode (selection: range) (node: TriviaNode) : TriviaNode option =
    let isExactSelection =
        selection.StartLine = node.Range.StartLine
        && selection.StartColumn = node.Range.StartColumn
        && selection.EndLine = node.Range.EndLine
        && selection.EndColumn = node.Range.EndColumn

    if isExactSelection then Some node else None

let mkAnonSynModuleOrNamespace decl =
    SynModuleOrNamespace(
        [],
        false,
        SynModuleOrNamespaceKind.AnonModule,
        [ decl ],
        PreXmlDoc.Empty,
        [],
        None,
        Range.Zero,
        { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
    )

let mkAnonSynModuleOrNamespaceSig decl =
    SynModuleOrNamespaceSig(
        [],
        false,
        SynModuleOrNamespaceKind.AnonModule,
        [ decl ],
        PreXmlDoc.Empty,
        [],
        None,
        Range.Zero,
        { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
    )

let mkSynModuleDecl (expr: SynExpr) : SynModuleDecl = SynModuleDecl.Expr(expr, expr.Range)

let mkSynModuleDeclForBinding (binding: SynBinding) : SynModuleDecl =
    SynModuleDecl.Let(false, [ binding ], binding.FullRange)

/// Wrap the selected node inside an anonymous module.
/// Keep the original trivia of the ParsedInput so code comments could still be restored.
let mkTreeWithSingleNode (fullTree: ParsedInput) (astNode: FSharpASTNode) : ParsedInput =
    match fullTree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (fileName,
                                                                     isScript,
                                                                     qualifiedNameOfFile,
                                                                     scopedPragmas,
                                                                     _hashDirectives,
                                                                     _modules,
                                                                     isLastCompiland,
                                                                     trivia)) ->
        let insertNode =
            match astNode with
            | FSharpASTNode.ModuleDecl synModuleDecl -> synModuleDecl
            | FSharpASTNode.Expr synExpr -> mkSynModuleDecl synExpr
            | FSharpASTNode.Binding binding -> mkSynModuleDeclForBinding binding
            | FSharpASTNode.ModuleSigDecl _
            | FSharpASTNode.ValSig _ -> failwith "Unexpected signature ast node in implementation file"

        ParsedInput.ImplFile(
            ParsedImplFileInput.ParsedImplFileInput(
                fileName,
                isScript,
                qualifiedNameOfFile,
                scopedPragmas,
                [],
                [ mkAnonSynModuleOrNamespace insertNode ],
                isLastCompiland,
                trivia
            )
        )
    | ParsedInput.SigFile (ParsedSigFileInput.ParsedSigFileInput (fileName,
                                                                  qualifiedNameOfFile,
                                                                  scopedPragmas,
                                                                  _hashDirectives,
                                                                  _sigDecls,
                                                                  trivia)) ->
        let insertNode =
            match astNode with
            | FSharpASTNode.ModuleSigDecl decl -> decl
            | FSharpASTNode.ValSig (SynValSig (range = range) as valSig) -> SynModuleSigDecl.Val(valSig, range)
            | FSharpASTNode.Expr _
            | FSharpASTNode.Binding _
            | FSharpASTNode.ModuleDecl _ -> failwith "Unexpected implementation ast node in implementation file"

        ParsedInput.SigFile(
            ParsedSigFileInput.ParsedSigFileInput(
                fileName,
                qualifiedNameOfFile,
                scopedPragmas,
                [],
                [ mkAnonSynModuleOrNamespaceSig insertNode ],
                trivia
            )
        )

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

        let rootNode =
            match baseUntypedTree with
            | ImplFile (ParsedImplFileInput (hds, mns, _, _)) -> astToNode baseUntypedTree.FullRange hds mns
            | SigFile (ParsedSigFileInput (_, mns, _, _)) -> sigAstToNode baseUntypedTree.FullRange mns

#if DEBUG
        printTriviaNode rootNode
#endif

        let selection = correctSelection rootNode.Range.FileIndex sourceText selection

        let treeWithSelection =
            findNodeWhereRangeFitsIn rootNode selection
            |> Option.bind (findNode selection)
            |> Option.bind (fun tna ->
                Option.map (fun astNode -> mkTreeWithSingleNode baseUntypedTree astNode, tna) tna.FSharpASTNode)

        if treeWithSelection.IsNone then
            raise (FormatException("No suitable AST node was found for the given selection."))

        let tree, node = treeWithSelection.Value
        let maxLineLength = config.MaxLineLength - selection.StartColumn

        let selectionConfig =
            { config with
                InsertFinalNewline = false
                MaxLineLength = maxLineLength }

        let selection =
            CodeFormatterImpl.formatAST tree (Some sourceText) selectionConfig (Some { Node = node })

        return selection.TrimEnd([| '\r'; '\n' |]), node.Range
    }
