#r "nuget: CliWrap"
#r "nuget: Thoth.Json.System.Text.Json"
#r "../../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"

open Fantomas.Core

// let st = Fantomas.Core.CodeFormatterImpl.getSourceText "let a = 0"
// printfn "%A" (st.GetType())
//
// open Fantomas.Core
// open Thoth.Json.Core
// open Thoth.Json.System.Text.Json
//
// type FormatEvents<'state> = Result<IEncodable list * 'state, IEncodable list>
//
// let encodeRange (range: Fantomas.FCS.Text.Range) =
//     Encode.object [
//         "startLine", Encode.int range.StartLine
//         "startColumn", Encode.int range.StartColumn
//         "endLine", Encode.int range.EndLine
//         "endColumn", Encode.int range.EndColumn
//     ]
//
// type InitialState = { Input: string; IsSignature: bool }
//
// let getSourceText input isSignatureFile : FormatEvents<InitialState> =
//     try
//         let _ = CodeFormatterImpl.getSourceText input
//
//         Ok(
//             [
//                 Encode.object [
//                     "name", Encode.string "Created SourceText"
//                     "description", Encode.string "Successfully created internal ISourceText value for input"
//                 ]
//             ],
//             {
//                 Input = input
//                 IsSignature = isSignatureFile
//             }
//         )
//
//     with ex ->
//         Error [
//             Encode.object [
//                 "name", Encode.string "Failed to create ISourceText"
//                 "description", Encode.string $"Could not construct  FCS.Text.ISourceText for \"{input}\""
//                 "exception", Encode.string (ex.ToString())
//             ]
//         ]
//
// [<NoComparison; NoEquality>]
// type StateWithUntypedTree = {
//     Input: string
//     IsSignature: bool
//     UntypedTrees: (Fantomas.FCS.Syntax.ParsedInput * string list) array
// }
//
// let parseUntypedTree (events, state: InitialState) =
//     async {
//         let type_ = if state.IsSignature then "signature" else "implementation"
//
//         try
//             let! untypedTrees = CodeFormatter.ParseAsync(state.IsSignature, state.Input)
//
//             let nextEvents =
//                 Encode.object [
//                     "name", Encode.string "Parsed input to Untyped AST"
//                     "description", Encode.string "Source input was able to be parsed as untyped AST"
//                     "untypedTrees",
//                     Encode.array (
//                         untypedTrees
//                         |> Array.map (fun (ast, defines) ->
//                             Encode.object [
//                                 "defines", Encode.list (List.map Encode.string defines)
//                                 "ast", Encode.string (string ast)
//                             ])
//                     )
//                 ]
//                 :: events
//
//             return
//                 Ok(
//                     nextEvents,
//                     {
//                         Input = state.Input
//                         IsSignature = state.IsSignature
//                         UntypedTrees = untypedTrees
//                     }
//                 )
//         with
//         | ParseException(diagnostics) ->
//             let nextEvents =
//                 Encode.object [
//                     "name", Encode.string "Parse exception raised during parsing of input to Untyped AST failed"
//                     "description", Encode.string $"Fantomas.FCS was unable to parse {type_} input"
//                     "input", Encode.string state.Input
//                     "diagnostics",
//                     diagnostics
//                     |> List.map (fun diag ->
//                         Encode.object [
//                             "severity", Encode.string (string diag.Severity)
//                             "subCategory", Encode.string diag.SubCategory
//                             "range", Encode.losslessOption encodeRange diag.Range
//                         ])
//                     |> Encode.list
//                 ]
//                 :: events
//
//             return Error(nextEvents)
//         | ex ->
//             let nextEvents =
//                 Encode.object [
//                     "name", Encode.string "Unexpected exception during parsing of input to Untyped AST failed"
//                     "description", Encode.string $"Fantomas.FCS was unable to parse {type_} input"
//                     "input", Encode.string state.Input
//                     "error", Encode.string (ex.ToString())
//                 ]
//                 :: events
//
//             return Error(nextEvents)
//     }
//
// let (>>=) f g =
//     async {
//         match f with
//         | Error e -> return Error e
//         | Ok(e, s) -> return! g (e, s)
//     }
//
// let format input isSignature : Async<string> =
//     async {
//         let! events = getSourceText input isSignature >>= parseUntypedTree
//
//         let orderedEvents =
//             match events with
//             | Error e -> e
//             | Ok(e, _) -> e
//             |> List.rev
//
//         let json = Encode.object [ "events", Encode.list orderedEvents ]
//
//         return Encode.toString 2 json
//     }
//

module OakPrinter =
    open Thoth.Json.Core
    open Thoth.Json.System.Text.Json
    open Fantomas.FCS.Text
    open Fantomas.Core.SyntaxOak

    let encodeRange (m: range) =
        Encode.object [
            "startLine", Encode.int m.StartLine
            "startColumn", Encode.int m.StartColumn
            "endLine", Encode.int m.EndLine
            "endColumn", Encode.int m.EndColumn
        ]

    let encodeTriviaNode (triviaNode: TriviaNode) : IEncodable =
        let contentType, content =
            match triviaNode.Content with
            | CommentOnSingleLine comment -> "commentOnSingleLine", Some comment
            | LineCommentAfterSourceCode comment -> "lineCommentAfterSourceCode", Some comment
            | BlockComment(comment, _, _) -> "blockComment", Some comment
            | Newline -> "newline", None
            | Directive directive -> "directive", Some directive
            | Cursor -> "cursor", None

        Encode.object [
            "range", encodeRange triviaNode.Range
            "type", Encode.string contentType
            "content", Encode.lossyOption Encode.string content
        ]

    let rec encodeNode (node: Node) (continuation: IEncodable -> IEncodable) : IEncodable =
        let continuations = List.map encodeNode (Array.toList node.Children)

        let text =
            match node with
            | :? SingleTextNode as stn ->
                if stn.Text.Length < 13 then
                    stn.Text
                else
                    sprintf "%s.." (stn.Text.Substring(0, 10))
                |> Some
            | _ -> None

        let finalContinuation (children: IEncodable list) =
            Encode.object [
                yield "type", Encode.string (node.GetType().Name)
                match text with
                | None -> ()
                | Some text -> yield ("text", Encode.string text)
                yield "range", encodeRange node.Range
                if node.HasContentBefore then
                    yield "contentBefore", Encode.seq (Seq.map encodeTriviaNode node.ContentBefore)
                if node.Children.Length > 0 then
                    yield "children", Encode.list children
                if node.HasContentAfter then
                    yield "contentAfter", Encode.seq (Seq.map encodeTriviaNode node.ContentAfter)
            ]
            |> continuation

        Continuation.sequence continuations finalContinuation

    let encodeOak (oak: Fantomas.Core.SyntaxOak.Oak) = encodeNode oak id |> Encode.toString 2

async {
    let input = stdin.ReadToEnd()

    let isSignature =
        System.Environment.GetCommandLineArgs() |> Array.contains "--signature"

    let mutable formatted = None

    MCPEvents.resetEvents ()

    try
        let! output = CodeFormatter.FormatDocumentAsync(isSignature, input)
        formatted <- Some(output.Code)
    with
    | ParseException(diags) -> MCPEvents.addEvent (MCPEvents.EventKind.ParseFailed(diags))
    | ex -> MCPEvents.addEvent (MCPEvents.EventKind.UnExpectedExceptionHappened(ex))

    let! isValid =
        match formatted with
        | None -> async { return false }
        | Some(formattedCode) -> CodeFormatter.IsValidFSharpCodeAsync(isSignature, formattedCode)

    let events =
        MCPEvents.capturedEvents
        |> Seq.map (fun event ->
            match event.Kind with
            | MCPEvents.EventKind.CreatedOakRaw oak ->
                string (
                    {
                        event with
                            Kind = MCPEvents.EventKind.CreatedOak(OakPrinter.encodeOak oak)
                    }
                )
            | _ -> string event)
        |> String.concat "\n\n"

    let formatted = formatted |> Option.defaultValue "<no result>"

    printfn
        $"Events:\n---\n{events}\n---\n\nResult is valid F# code: {isValid}\n---\n\nFormatted code:\n---\n{formatted}---"
}
|> Async.RunSynchronously




// TODO: don't just dump a response, we should create some event sourced response.
// Numerous things happen here; code builds, input was parsed, trivia detected code formatted, result validated, result trivia detected, etc.
// We need to return this as JSON or some other structured format.
// See https://t3.chat/chat/76da167e-f594-4b3a-9f11-3facb9c11b5e
