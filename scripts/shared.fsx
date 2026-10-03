#r "../artifacts/bin/Fantomas.FCS/debug/Fantomas.FCS.dll"
#r "../artifacts/bin/Fantomas.Core/debug/Fantomas.Core.dll"
#r "../artifacts/bin/Fantomas.EditorConfig/debug/Fantomas.EditorConfig.dll"
// The snapshot tests, for reading a case and for formatting and checking it the way a case is:
// referenced as built, because their checks use internals of Fantomas.Core a script cannot see.
#r "../artifacts/bin/Fantomas.Core.SnapshotTests/debug/Fantomas.Core.SnapshotTests.dll"
// Must match the version `Directory.Packages.props` gives Fantomas: `EditorConfigFiles.fs` is
// loaded as source below and compiles against whatever this resolves.
#r "nuget: editorconfig, 0.18.0"

#load "../src/Fantomas/EditorConfigFiles.fs"

open System.IO
open Fantomas.Core
open Fantomas.EditorConfigFiles

let parseEditorConfigContent (content: string) : FormatConfig =
    let tempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
    Directory.CreateDirectory(tempDir) |> ignore
    let editorConfigPath = Path.Combine(tempDir, ".editorconfig")
    let fsharpFile = Path.Combine(tempDir, "temp.fs")
    File.WriteAllText(editorConfigPath, $"root = true\n\n[*.fs]\n%s{content}")
    File.WriteAllText(fsharpFile, "")

    // What the tool reads, except that `end_of_line` is `lf` unless the content says otherwise, as
    // it is for a snapshot case: `FormatConfig.Default` follows the machine.
    let setsEndOfLine: bool =
        System.Text.RegularExpressions.Regex.IsMatch(
            content,
            @"^\s*end_of_line\s*=",
            System.Text.RegularExpressions.RegexOptions.IgnoreCase
            ||| System.Text.RegularExpressions.RegexOptions.Multiline
        )

    try
        let config: FormatConfig =
            match tryReadConfiguration fsharpFile with
            | Some result -> result.Config
            | None -> FormatConfig.Default

        if setsEndOfLine then
            config
        else

        { config with
            EndOfLine = EndOfLineStyle.LF
        }
    finally
        Directory.Delete(tempDir, true)

/// Parses args and returns (source, isSignature, config, defines).
/// Accepts either a file path as last arg, or source code via stdin.
/// Optional flags: --editorconfig <content>, --signature, --define FOO,BAR
/// A snapshot case, a file that starts with `(*---` front matter, is read without its front matter,
/// and the front matter gives the config unless `--editorconfig` does.
let parseArgs (args: string array) =
    let editorConfigIdx = args |> Array.tryFindIndex (fun a -> a = "--editorconfig")
    let hasSignatureFlag = args |> Array.exists (fun a -> a = "--signature")
    let defineIdx = args |> Array.tryFindIndex (fun a -> a = "--define")

    // Without settings, the ones a snapshot case formats with: `end_of_line` is `lf` there, where
    // `FormatConfig.Default` follows the machine.
    let config =
        match editorConfigIdx with
        | Some idx -> parseEditorConfigContent args.[idx + 1]
        | None -> Fantomas.Core.SnapshotTests.Case.defaultConfig

    let defines =
        match defineIdx with
        | Some idx -> args.[idx + 1].Split(',') |> Array.toList
        | None -> []

    // Collect flag indices to determine which arg (if any) is the input file
    let flagIndices =
        [|
            match editorConfigIdx with
            | None -> ()
            | Some idx ->
                yield idx
                yield idx + 1
            match defineIdx with
            | None -> ()
            | Some idx ->
                yield idx
                yield idx + 1
            yield!
                args
                |> Array.indexed
                |> Array.choose (fun (i, a) -> if a = "--signature" then Some i else None)
        |]

    let positionalArgs =
        args
        |> Array.indexed
        |> Array.filter (fun (i, _) -> not (Array.contains i flagIndices))
        |> Array.map snd

    match Array.tryLast positionalArgs with
    | Some path when File.Exists(path) ->
        let properties, sample =
            Fantomas.Core.SnapshotTests.Case.splitFrontMatter ((File.ReadAllText path).Replace("\r\n", "\n"))

        let config: FormatConfig =
            if properties.IsEmpty || editorConfigIdx.IsSome then
                config
            else
                Fantomas.Core.SnapshotTests.Case.configOf properties

        let isSignature = hasSignatureFlag || path.EndsWith(".fsi")
        sample, isSignature, config, defines
    | Some path ->
        // Read stdin instead and a mistyped path waits for input, or formats nothing.
        eprintfn $"No such file: %s{path}"
        exit 1
    | None ->
        // With `\n` line endings, as a case file is read, whatever the terminal sends.
        let sample: string = stdin.ReadToEnd().Replace("\r\n", "\n")
        sample, hasSignatureFlag, config, defines
