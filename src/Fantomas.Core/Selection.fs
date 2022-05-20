module Fantomas.Core.Selection

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Core.FormatConfig
open Fantomas.Core.SourceParser
open Fantomas.Core.TriviaTypes
open Fantomas.Core.AstTransformer

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
    let insertNode =
        match astNode with
        | Choice1Of2 synModuleDecl -> synModuleDecl
        | Choice2Of2 synExpr -> mkSynModuleDecl synExpr

    match fullTree with
    | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (fileName,
                                                                     isScript,
                                                                     qualifiedNameOfFile,
                                                                     scopedPragmas,
                                                                     _hashDirectives,
                                                                     _modules,
                                                                     isLastCompiland,
                                                                     trivia)) ->
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
            return None
        else
            let triviaNodes =
                match baseUntypedTree with
                | ImplFile (ParsedImplFileInput (hds, mns, _, _)) -> astToNode hds mns
                | SigFile (ParsedSigFileInput (_, mns, _, _)) -> sigAstToNode mns
                |> List.sortBy (fun n -> n.Range.Start.Line, n.Range.Start.Column)

            let treeWithSelection =
                triviaNodes
                |> List.filter (fun tna ->
                    tna.HasFSharpASTNode
                    && RangeHelpers.``range contains`` selection tna.Range)
                |> List.sortByDescending (fun tna ->
                    if tna.Range.StartLine = tna.Range.EndLine then
                        tna.Range.EndColumn - tna.Range.StartColumn
                    else
                        // Calculate an artificial surface of positions they range consume.
                        // Take the max_line_length as size for a blank line
                        // This isn't totally accurate, but will do the trick.
                        // The larger the surface, the closer to the selection the node is.
                        let linesInBetween =
                            match [ tna.Range.StartLine + 1 .. tna.Range.EndLine - 1 ] with
                            | []
                            | [ _ ] -> 0
                            | lines -> lines.Length * config.MaxLineLength

                        (config.MaxLineLength - tna.Range.StartColumn)
                        + linesInBetween
                        + tna.Range.EndColumn)
#if DEBUG
                |> fun tap ->
                    printfn "sorted: %A" tap
                    tap
#endif
                |> List.tryHead
                |> Option.bind (fun tna -> Option.map (mkTreeWithSingleNode baseUntypedTree) tna.FSharpASTNode)

            match treeWithSelection with
            | None -> return None
            | Some tree ->
                let maxLineLength = config.MaxLineLength - selection.StartColumn

                let selectionConfig =
                    { config with
                        InsertFinalNewline = false
                        MaxLineLength = maxLineLength }

                let selection =
                    CodeFormatterImpl.formatAST tree [] (Some sourceText) selectionConfig

                return Some(selection.TrimEnd([| '\r'; '\n' |]))
    }
