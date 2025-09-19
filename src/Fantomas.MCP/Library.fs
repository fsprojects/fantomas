module Fantomas.MCP

open Fantomas.Core
open Thoth.Json.Core
open Thoth.Json.System.Text.Json

type FormatEvents<'state> = Result<IEncodable list * 'state, IEncodable list>

let encodeRange (range: Fantomas.FCS.Text.Range) =
    Encode.object [
        "startLine", Encode.int range.StartLine
        "startColumn", Encode.int range.StartColumn
        "endLine", Encode.int range.EndLine
        "endColumn", Encode.int range.EndColumn
    ]

type InitialState = { Input: string; IsSignature: bool }

let getSourceText input isSignatureFile : FormatEvents<InitialState> =
    try
        let _ = CodeFormatterImpl.getSourceText input

        Ok(
            [
                Encode.object [
                    "name", Encode.string "Created SourceText"
                    "description", Encode.string "Successfully created internal ISourceText value for input"
                ]
            ],
            {
                Input = input
                IsSignature = isSignatureFile
            }
        )

    with ex ->
        Error [
            Encode.object [
                "name", Encode.string "Failed to create ISourceText"
                "description", Encode.string $"Could not construct  FCS.Text.ISourceText for \"{input}\""
                "exception", Encode.string (ex.ToString())
            ]
        ]

[<NoComparison; NoEquality>]
type StateWithUntypedTree = {
    Input: string
    IsSignature: bool
    UntypedTrees: (FCS.Syntax.ParsedInput * string list) array
}

let parseUntypedTree (events, state: InitialState) =
    async {
        let type_ = if state.IsSignature then "signature" else "implementation"

        try
            let! untypedTrees = CodeFormatter.ParseAsync(state.IsSignature, state.Input)

            let nextEvents =
                Encode.object [
                    "name", Encode.string "Parsed input to Untyped AST"
                    "description", Encode.string "Source input was able to be parsed as untyped AST"
                    "untypedTrees",
                    Encode.array (
                        untypedTrees
                        |> Array.map (fun (ast, defines) ->
                            Encode.object [
                                "defines", Encode.list (List.map Encode.string defines)
                                "ast", Encode.string (string ast)
                            ])
                    )
                ]
                :: events

            return
                Ok(
                    nextEvents,
                    {
                        Input = state.Input
                        IsSignature = state.IsSignature
                        UntypedTrees = untypedTrees
                    }
                )
        with
        | ParseException(diagnostics) ->
            let nextEvents =
                Encode.object [
                    "name", Encode.string "Parse exception raised during parsing of input to Untyped AST failed"
                    "description", Encode.string $"Fantomas.FCS was unable to parse {type_} input"
                    "input", Encode.string state.Input
                    "diagnostics",
                    diagnostics
                    |> List.map (fun diag ->
                        Encode.object [
                            "severity", Encode.string (string diag.Severity)
                            "subCategory", Encode.string diag.SubCategory
                            "range", Encode.losslessOption encodeRange diag.Range
                        ])
                    |> Encode.list
                ]
                :: events

            return Error(nextEvents)
        | ex ->
            let nextEvents =
                Encode.object [
                    "name", Encode.string "Unexpected exception during parsing of input to Untyped AST failed"
                    "description", Encode.string $"Fantomas.FCS was unable to parse {type_} input"
                    "input", Encode.string state.Input
                    "error", Encode.string (ex.ToString())
                ]
                :: events

            return Error(nextEvents)
    }

let (>>=) f g =
    async {
        match f with
        | Error e -> return Error e
        | Ok(e, s) -> return! g (e, s)
    }

let format input isSignature : Async<string> =
    async {
        let! events = getSourceText input isSignature >>= parseUntypedTree

        let orderedEvents =
            match events with
            | Error e -> e
            | Ok(e, _) -> e
            |> List.rev

        let json = Encode.object [ "events", Encode.list orderedEvents ]

        return Encode.toString 2 json
    }
