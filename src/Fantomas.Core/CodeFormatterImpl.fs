[<RequireQualifiedAccess>]
module internal Fantomas.Core.CodeFormatterImpl

open Fantomas.FCS.Diagnostics
open Fantomas.FCS.Syntax
open Fantomas.FCS.Text
open MultipleDefineCombinations

let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<(ParsedInput * DefineCombination) array> =
    // First get the syntax tree without any defines
    let baseUntypedTree, baseDiagnostics =
        Fantomas.FCS.Parse.parseFile isSignature source []

    let hashDirectives =
        match baseUntypedTree with
        | ParsedInput.ImplFile(ParsedImplFileInput(trivia = { ConditionalDirectives = directives }))
        | ParsedInput.SigFile(ParsedSigFileInput(trivia = { ConditionalDirectives = directives })) -> directives

    match hashDirectives with
    | [] ->
        async {
            let errors =
                baseDiagnostics
                |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

            if not errors.IsEmpty then
                raise (ParseException baseDiagnostics)

            return [| (baseUntypedTree, DefineCombination.Empty) |]
        }
    | hashDirectives ->
        let defineCombinations = Defines.getDefineCombination hashDirectives

        async {
            let! results =
                defineCombinations
                |> List.map (fun defineCombination ->
                    async {
                        let untypedTree, diagnostics =
                            Fantomas.FCS.Parse.parseFile isSignature source defineCombination.Value

                        let errors =
                            diagnostics
                            |> List.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)

                        if errors.IsEmpty then
                            return Ok(untypedTree, defineCombination)
                        else
                            let defineNames =
                                if defineCombination.Value.IsEmpty then
                                    "no defines"
                                else
                                    defineCombination.Value |> String.concat ", "

                            return Error defineNames
                    })
                |> Async.Parallel

            let failures =
                results
                |> Array.choose (function
                    | Error name -> Some name
                    | _ -> None)
                |> Array.toList

            if not failures.IsEmpty then
                raise (DefineParseException(failures))

            return
                results
                |> Array.choose (function
                    | Ok result -> Some result
                    | _ -> None)
        }

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

    let oak =
        match cursor with
        | None -> oak
        | Some cursor -> Trivia.insertCursor oak cursor

    context |> CodePrinter.genFile oak |> Context.dump false

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
