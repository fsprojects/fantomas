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

let private findNode (selection: range) (node: TriviaNode) : TriviaNode option =
    let isExactSelection =
        selection.StartLine = node.Range.StartLine
        && selection.StartColumn = node.Range.StartColumn
        && selection.EndLine = node.Range.EndLine
        && selection.EndColumn = node.Range.EndColumn

    if isExactSelection then
        Some node
    else
        None

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
    : Async<(string * range) option> =
    async {
        let baseUntypedTree, baseDiagnostics =
            Fantomas.FCS.Parse.parseFile isSignature sourceText []

        let isValid = Validation.noWarningOrErrorDiagnostics baseDiagnostics

        if not isValid then
            failwith "Format selection cannot work unless the entire tree is valid."
            return None
        else
            let rootNode =
                match baseUntypedTree with
                | ImplFile (ParsedImplFileInput (hds, mns, _, _)) -> astToNode baseUntypedTree.FullRange hds mns
                | SigFile (ParsedSigFileInput (_, mns, _, _)) -> sigAstToNode baseUntypedTree.FullRange mns

#if DEBUG
            printTriviaNode rootNode
#endif

            let selection =
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
                            Seq.takeWhile System.Char.IsWhiteSpace startLine
                            |> Seq.length

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

                    if startLineNumber <> selection.StartLine
                       || startColumn <> selection.StartColumn
                       || endLineNumber <> selection.EndLine
                       || endColumn <> selection.EndColumn then

                        Range.mkFileIndexRange
                            rootNode.Range.FileIndex
                            (Position.mkPos startLineNumber startColumn)
                            (Position.mkPos endLineNumber endColumn)
                    else
                        selection
                | _ -> selection

            let treeWithSelection =
                findNodeWhereRangeFitsIn rootNode selection
                |> Option.bind (findNode selection)
                |> Option.bind (fun tna ->
                    Option.map (fun astNode -> mkTreeWithSingleNode baseUntypedTree astNode, tna) tna.FSharpASTNode)

            match treeWithSelection with
            | None ->
                failwithf "no node found, %A %A" selection baseUntypedTree
                return None
            | Some (tree, node) ->
                let maxLineLength = config.MaxLineLength - selection.StartColumn

                let selectionConfig =
                    { config with
                        InsertFinalNewline = false
                        MaxLineLength = maxLineLength }

                let selection =
                    CodeFormatterImpl.formatAST tree (Some sourceText) selectionConfig (Some { Node = node })

                return Some(selection.TrimEnd([| '\r'; '\n' |]), node.Range)
    }
