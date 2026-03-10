#load "shared.fsx"

open System.IO
open Fantomas.Core
open Shared

let parseOak (input: string) (isSignature: bool) (defines: string list) =
    async {
        try
            let! oaks = CodeFormatter.ParseOakAsync(isSignature, input)

            let result =
                if List.isEmpty defines then
                    Array.tryHead oaks
                else
                    let sortedDefines = List.sort defines
                    oaks |> Array.tryFind (fun (_, d) -> List.sort d = sortedDefines)

            match result with
            | None -> return "No Oak found in input"
            | Some(oak, _) -> return (string oak)
        with ex ->
            return $"Error while parsing to Oak: %A{ex}"
    }

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample, isSignature, _, defines = parseArgs fsi.CommandLineArgs.[1..]
        parseOak sample isSignature defines |> Async.RunSynchronously |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi oak.fsx [--signature] [--define FOO,BAR] <input file>"
