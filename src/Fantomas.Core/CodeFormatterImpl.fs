[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open MultipleDefineCombinations

let getSourceText (source: string) : ISourceText =
    MCPEvents.addEvent MCPEvents.EventKind.SourceTextCreated
    source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<(ParsedInput * DefineCombination) array> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    let hashDirectives =
        match baseUntypedTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

    let errors =
        baseDiagnostics
        |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

    if not errors.IsEmpty then
        raise (ParseException baseDiagnostics)

    MCPEvents.addEvent (MCPEvents.EventKind.ParsedBaseUntypedTree(baseUntypedTree, baseDiagnostics))

    match hashDirectives with
    | [] -> async { return [| (baseUntypedTree, DefineCombination.Empty) |] }
    | hashDirectives ->
        let defineCombinations = Defines.getDefineCombination hashDirectives
        MCPEvents.addEvent (MCPEvents.EventKind.SourceCodeHadDefines defineCombinations)

        defineCombinations
        |> List.map (fun defineCombination ->
            async {
                let untypedTree, diagnostics =
                    Fantomas.FCS.Parse.parseFile isSignature source defineCombination.Value

                let errors =
                    diagnostics
                    |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                if not errors.IsEmpty then
                    raise (ParseException diagnostics)

                MCPEvents.addEvent (
                    MCPEvents.EventKind.ParsedUntypedTreeWithDefines(untypedTree, diagnostics, defineCombination)
                )

                return (untypedTree, defineCombination)
            })
        |> Async.Parallel

/// Format an abstract syntax tree using given config
let formatAST
    (ast: ParsedInput)
    (sourceText: ISourceText option)
    (config: FormatConfig)
    (cursor: pos option)
    : FormatResult =
    let context = Context.Context.Create config

    let oak =
        match sourceText with
        | None -> ASTTransformer.mkOak None ast
        | Some sourceText ->
            ASTTransformer.mkOak (Some sourceText) ast
            |> Trivia.enrichTree config sourceText ast

    MCPEvents.addEvent (MCPEvents.EventKind.CreatedOakRaw oak)

    let oak =
        match cursor with
        | None -> oak
        | Some cursor -> Trivia.insertCursor oak cursor

    let contextAfter = context |> CodePrinter.genFile oak

    MCPEvents.addEvent (
        contextAfter.WriterEvents
        |> Seq.map string<Context.WriterEvent>
        |> String.concat " , "
        |> MCPEvents.CollectedEventsAfterCodePrinter
    )

    contextAfter |> Context.dump false

let formatDocument
    (config: FormatConfig)
    (isSignature: bool)
    (source: ISourceText)
    (cursor: pos option)
    : Async<FormatResult> =
    async {
        let! asts = parse isSignature source

        let! results =
            asts
            |> Array.map (fun (ast', defineCombination) ->
                async {
                    let formattedCode = formatAST ast' (Some source) config cursor
                    return (defineCombination, formattedCode)
                })
            |> Async.Parallel
            |> Async.map Array.toList

        let merged =
            match results with
            | [] -> failwith "not possible"
            | [ (_, x) ] -> x
            | all -> mergeMultipleFormatResults config all

        return merged
    }
