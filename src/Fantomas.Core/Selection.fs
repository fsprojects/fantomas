module Fantomas.Core.Selection

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.AstExtensions
open Fantomas.Core.TriviaTypes
open Fantomas.Core.AstTransformer
open Fantomas.Core.Trivia

let private findNode (maxLineLength: int) (selection: range) (node: TriviaNodeAssigner) : TriviaNodeAssigner option =
    let isExactSelection =
        selection.StartLine = node.Range.StartLine
        && selection.StartColumn = node.Range.StartColumn
        && selection.EndLine = node.Range.EndLine
        && selection.EndColumn = node.Range.EndColumn

    if isExactSelection then
        Some node
    else
        let selectionSurface = RangeHelpers.surfaceArea maxLineLength selection

        [| yield node; yield! node.Children |]
        |> Array.filter (fun a -> Option.isSome a.FSharpASTNode)
        |> Array.sortBy (fun n ->
            // Find the node that matches the selection as close as possible
            let nodeSurface = RangeHelpers.surfaceArea maxLineLength n.Range
            System.Math.Abs(selectionSurface - nodeSurface))
        |> Array.tryHead

let private mkAnonSynModuleOrNamespace decl =
    SynModuleOrNamespace(
        [],
        false,
        SynModuleOrNamespaceKind.AnonModule,
        [ decl ],
        PreXmlDoc.Empty,
        [],
        None,
        Range.Zero,
        { ModuleKeyword = None
          NamespaceKeyword = None }
    )

let private mkSynModuleDecl (expr: SynExpr) : SynModuleDecl = SynModuleDecl.Expr(expr, expr.Range)

/// Wrap the selected node inside an anonymous module
/// Keep the original trivia of the ParsedInput so code comments could still be restored.
let private mkTreeWithSingleNode (fullTree: ParsedInput) (astNode: FSharpASTNode) : ParsedInput =
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
            | Choice1Of3 synModuleDecl -> synModuleDecl
            | Choice2Of3 _ -> failwith "Unexpected signature module declaration in implementation file"
            | Choice3Of3 synExpr -> mkSynModuleDecl synExpr

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
    | ParsedInput.SigFile _sigFile -> fullTree

let formatSelection
    (config: FormatConfig)
    (isSignature: bool)
    (selection: range)
    (sourceText: ISourceText)
    : Async<string option> =
    async {
        let baseUntypedTree, baseDiagnostics =
            Fantomas.FCS.Parse.parseFile isSignature sourceText []

        let isValid = Validation.noWarningOrErrorDiagnostics baseDiagnostics

        if not isValid then
            failwith "not valid"
            return None
        else
            let rootNode =
                match baseUntypedTree with
                | ImplFile (ParsedImplFileInput (hds, mns, _, _)) -> astToNode baseUntypedTree.FullRange hds mns
                | SigFile (ParsedSigFileInput (_, mns, _, _)) -> sigAstToNode baseUntypedTree.FullRange mns

#if DEBUG
            printTriviaNode rootNode
#endif

            let treeWithSelection =
                findNodeWhereRangeFitsIn rootNode selection
                |> Option.bind (findNode config.MaxLineLength selection)
                |> Option.bind (fun tna -> Option.map (mkTreeWithSingleNode baseUntypedTree) tna.FSharpASTNode)

            match treeWithSelection with
            | None ->
                failwithf "no node found, %A %A" selection baseUntypedTree
                return None
            | Some tree ->
                let maxLineLength = config.MaxLineLength - selection.StartColumn

                let selectionConfig =
                    { config with
                        InsertFinalNewline = false
                        MaxLineLength = maxLineLength }

                let selection =
                    CodeFormatterImpl.formatAST
                        tree
                        (Some sourceText)
                        selectionConfig
                        (Some
                            { RootNode = rootNode
                              Selection = selection })

                return Some(selection.TrimEnd([| '\r'; '\n' |]))
    }
