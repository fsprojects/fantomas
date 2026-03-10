#load "shared.fsx"

open System.IO
open Shared

let parseAst (input: string) (isSignature: bool) (defines: string list) =
    try
        let ast =
            Fantomas.FCS.Parse.parseFile isSignature (Fantomas.FCS.Text.SourceText.ofString input) defines
            |> fst

        $"%A{ast}"
    with ex ->
        $"Error while parsing AST: %A{ex}"

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, _, defines = parseArgs fsi.CommandLineArgs.[1..]
        parseAst sample isSignature defines |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi ast.fsx [--signature] [--define FOO,BAR] <input file>"
