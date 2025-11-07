#r "nuget: Thoth.Json.System.Text.Json"
#r "../../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"

open Fantomas.Core
open Fantomas.FCS.Text
open Thoth.Json.Core
open Thoth.Json.System.Text.Json

let encodeRange (range: range) =
    Encode.object [
        "startLine", Encode.int range.StartLine
        "startColumn", Encode.int range.StartColumn
        "endLine", Encode.int range.EndLine
        "endColumn", Encode.int range.EndColumn
    ]

let encodeDiagnostic (diag: Fantomas.FCS.Parse.FSharpParserDiagnostic) =
    Encode.object [
        "severity", Encode.string (string diag.Severity)
        "subCategory", Encode.string diag.SubCategory
        "range", Encode.losslessOption encodeRange diag.Range
        "errorNumber", Encode.lossyOption Encode.int diag.ErrorNumber
        "message", Encode.string diag.Message
    ]

let encodeParsedInput (ast: Fantomas.FCS.Syntax.ParsedInput, defines: string list) =
    Encode.object [
        "defines", Encode.list (List.map Encode.string defines)
        "ast", Encode.string (string ast)
    ]

async {
    let input = stdin.ReadToEnd()

    let isSignature =
        System.Environment.GetCommandLineArgs() |> Array.contains "--signature"

    let fileType = if isSignature then "signature" else "implementation"

    try
        let! untypedTrees = CodeFormatter.ParseAsync(isSignature, input)

        let result =
            Encode.object [
                "success", Encode.bool true
                "fileType", Encode.string fileType
                "untypedTrees",
                Encode.array (Array.map encodeParsedInput untypedTrees)
                "diagnostics", Encode.list [] // No errors if we got here
            ]
            |> Encode.toString 2

        printfn $"Parsed {fileType} file successfully:\n---\n{result}\n---"
    with
    | ParseException(diagnostics) ->
        let result =
            Encode.object [
                "success", Encode.bool false
                "fileType", Encode.string fileType
                "error", Encode.string $"Fantomas.FCS was unable to parse {fileType} input"
                "input", Encode.string input
                "diagnostics", Encode.list (List.map encodeDiagnostic diagnostics)
            ]
            |> Encode.toString 2

        printfn $"Parse exception raised:\n---\n{result}\n---"
    | ex ->
        let result =
            Encode.object [
                "success", Encode.bool false
                "fileType", Encode.string fileType
                "error", Encode.string $"Unexpected exception during parsing of {fileType} input"
                "exception", Encode.string (ex.ToString())
            ]
            |> Encode.toString 2

        printfn $"Unexpected exception:\n---\n{result}\n---"
}
|> Async.RunSynchronously

