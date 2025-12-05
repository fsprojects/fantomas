#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"

open System.IO

match Array.tryHead fsi.CommandLineArgs with
| Some scriptPath ->
    let scriptFile = FileInfo(scriptPath)
    let sourceFile = FileInfo(Path.Combine(__SOURCE_DIRECTORY__, __SOURCE_FILE__))

    if scriptFile.FullName = sourceFile.FullName then
        let inputPath = fsi.CommandLineArgs.[fsi.CommandLineArgs.Length - 1]
        let sample = File.ReadAllText(inputPath)
        let isSignature = inputPath.EndsWith(".fsi")

        let ast =
            Fantomas.FCS.Parse.parseFile isSignature (Fantomas.FCS.Text.SourceText.ofString sample) []
            |> fst

        ast |> printfn "%A"
| _ -> printfn "Usage: dotnet fsi ast.fsx <input file>"
