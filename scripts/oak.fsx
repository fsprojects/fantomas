#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"

open System.Threading.Tasks
open Fantomas.Core

let parseOak (input: string) (isSignature: bool) : Task<string> =
    task {
        try
            let! oaks = CodeFormatter.ParseOakAsync(isSignature, input)

            match Array.tryHead oaks with
            | None -> return "No Oak found in input"
            | Some(oak, _) -> return (string oak)

        with ex ->
            return $"Error while parsing to Oak: %A{ex}"
    }

open System.IO

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let sample = File.ReadAllText(fsi.CommandLineArgs.[fsi.CommandLineArgs.Length - 1])
        let isSignature = sample.EndsWith(".fsi")

        parseOak sample isSignature
        |> Async.AwaitTask
        |> Async.RunSynchronously
        |> printfn "%s"
| _ -> printfn "Usage: dotnet fsi oak.fsx <input file>"
