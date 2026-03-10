#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"
#r "nuget: editorconfig"

#load "../src/Fantomas/EditorConfig.fs"

open System.IO
open Fantomas.Core
open Fantomas.EditorConfig

let parseEditorConfigContent (content: string) : FormatConfig =
    let tempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    Directory.CreateDirectory(tempDir) |> ignore
    let editorConfigPath = Path.Combine(tempDir, ".editorconfig")
    let fsharpFile = Path.Combine(tempDir, "temp.fs")
    File.WriteAllText(editorConfigPath, $"root = true\n\n[*.fs]\n%s{content}")
    File.WriteAllText(fsharpFile, "")

    try
        readConfiguration fsharpFile
    finally
        Directory.Delete(tempDir, true)

/// Parses args and returns (source, isSignature, config).
/// Accepts either a file path as last arg, or source code via stdin.
/// Optional flags: --editorconfig <content>, --signature
let parseArgs (args: string array) =
    let editorConfigIdx = args |> Array.tryFindIndex (fun a -> a = "--editorconfig")
    let hasSignatureFlag = args |> Array.exists (fun a -> a = "--signature")
    let defineIdx = args |> Array.tryFindIndex (fun a -> a = "--define")

    let config =
        match editorConfigIdx with
        | Some idx -> parseEditorConfigContent args.[idx + 1]
        | None -> FormatConfig.Default

    let defines =
        match defineIdx with
        | Some idx -> args.[idx + 1].Split(',') |> Array.toList
        | None -> []

    // Collect flag indices to determine which arg (if any) is the input file
    let flagIndices =
        [| match editorConfigIdx with
           | Some idx ->
               yield idx
               yield idx + 1
           | None -> ()
           match defineIdx with
           | Some idx ->
               yield idx
               yield idx + 1
           | None -> ()
           yield!
               args
               |> Array.indexed
               |> Array.choose (fun (i, a) -> if a = "--signature" then Some i else None) |]

    let positionalArgs =
        args
        |> Array.indexed
        |> Array.filter (fun (i, _) -> not (Array.contains i flagIndices))
        |> Array.map snd

    match Array.tryLast positionalArgs with
    | Some path when File.Exists(path) ->
        let sample = File.ReadAllText(path)
        let isSignature = hasSignatureFlag || path.EndsWith(".fsi")
        sample, isSignature, config, defines
    | _ ->
        let sample = stdin.ReadToEnd()
        sample, hasSignatureFlag, config, defines
