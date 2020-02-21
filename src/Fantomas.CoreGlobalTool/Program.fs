module Fantomas.Cmd.Program

open System
open System.IO
open Fantomas
open Fantomas.FormatConfig
open Argu

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli"; |]

type Arguments =
    | [<Unique>] Recurse
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Fsi of string
    | [<Unique>] Stdin
    | [<Unique>] Stdout
    | [<Unique>] Out of string
    | [<Unique>] Indent of int
    | [<Unique;AltCommandLine("--pageWidth")>] PageWidth of int
    | [<Unique;AltCommandLine("--semicolonEOL")>] SemicolonEOL
    | [<Unique;AltCommandLine("--noSpaceBeforeArgument")>] NoSpaceBeforeArgument
    | [<Unique;AltCommandLine("--spaceBeforeColon")>] SpaceBeforeColon
    | [<Unique;AltCommandLine("--noSpaceAfterComma")>] NoSpaceAfterComma
    | [<Unique;AltCommandLine("--noSpaceAfterSemiColon")>] NoSpaceAfterSemiColon
    | [<Unique;AltCommandLine("--indentOnTryWith")>] IndentOnTryWith
    | [<Unique;AltCommandLine("--reorderOpenDeclaration")>] ReorderOpenDeclaration
    | [<Unique;AltCommandLine("--noSpaceAroundDelimiter")>] NoSpaceAroundDelimiter
    | [<Unique;AltCommandLine("--keepNewlineAfter")>] KeepNewlineAfter
    | [<Unique;AltCommandLine("--maxIfThenElseShortWidth ")>] MaxIfThenElseShortWidth of int
    | [<Unique;AltCommandLine("--strictMode")>] StrictMode
    | [<Unique;AltCommandLine("--emptyLineBeforeNestedModuleBody")>] EmptyLineBeforeNestedModuleBody
    | [<Unique;AltCommandLine("-c")>] Config of string
    | [<Unique;AltCommandLine("-v")>] Version
    | [<MainCommand>] Input of string
with
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recurse -> "Process the input folder recursively."
            | Force -> "Print the source unchanged if it cannot be parsed correctly."
            | Out _ -> "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only."
            | Profile -> "Print performance profiling information."
            | Fsi _ -> "Read F# source from stdin as F# signatures."
            | Stdin -> "Read F# source from standard input."
            | Stdout -> " Write the formatted source code to standard output."
            | Indent _ -> "Set number of spaces for indentation (default = 4). The value should be between 1 and 10."
            | PageWidth _ -> "Set the column where we break to new lines (default = 80). The value should be at least 60."
            | SemicolonEOL -> "Enable semicolons at the end of line (default = false)."
            | NoSpaceBeforeArgument -> "Disable spaces before the first argument of functions when there are parenthesis (default = true). For methods and constructors, there are never spaces regardless of this option."
            | SpaceBeforeColon -> "Enable spaces before colons (default = false)."
            | NoSpaceAfterComma -> "Disable spaces after commas (default = true)."
            | NoSpaceAfterSemiColon -> "Disable spaces after semicolons (default = true)."
            | IndentOnTryWith -> "Enable indentation on try/with block (default = false)."
            | ReorderOpenDeclaration -> "[DEPRECATED] Enable reordering open declarations (default = false)."
            | NoSpaceAroundDelimiter -> "Disable spaces after starting and before ending of lists, arrays, sequences and records (default = true)."
            | KeepNewlineAfter -> "Keep newlines found after = in let bindings, -> in pattern matching and chained function calls (default = false)."
            | MaxIfThenElseShortWidth _ -> "Set the max length of any expression in an if expression before formatting on multiple lines (default = 40)."
            | StrictMode -> "Enable strict mode (ignoring directives and comments and printing literals in canonical forms) (default = false)."
            | EmptyLineBeforeNestedModuleBody -> "Add an empty line before the body of a nested module"
            | Config _ -> "Use configuration found in file or folder."
            | Version -> "Displays the version of Fantomas"
            | Input _ -> sprintf "Input path: can be a folder or file with %s extension." (Seq.map (fun s -> "*" + s) extensions |> String.concat ",")

let time f =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = f()
    sw.Stop()
    printfn "Time taken: %O s" sw.Elapsed
    res

[<RequireQualifiedAccess>]
type InputPath = 
    | File of string 
    | Folder of string 
    | StdIn of string
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | StdOut
    | Notknown

let isInExcludedDir (fullPath: string) =
    set [| "obj"; ".fable"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists (fun dir -> fullPath.Contains(dir))

let isFSharpFile (s: string) = Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    let searchOption = (if isRec then SearchOption.AllDirectories else SearchOption.TopDirectoryOnly)
    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

/// Format a source string using given config and write to a text writer
let processSourceString isFsiFile s (tw : Choice<TextWriter, string>) config =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    async {
        let! formatted = CodeFormatter.FormatDocumentAsync(fileName, SourceOrigin.SourceString s, config,
                                                           FakeHelpers.createParsingOptionsFromFile fileName,
                                                           FakeHelpers.sharedChecker.Value)
        match tw with
        | Choice1Of2 tw -> tw.Write(formatted)
        | Choice2Of2 path -> File.WriteAllText(path, formatted)
    }
    |> Async.RunSynchronously

/// Format inFile and write to text writer
let processSourceFile inFile (tw : TextWriter) config = 
    let s = File.ReadAllText(inFile)
    async {
        let! formatted = CodeFormatter.FormatDocumentAsync(inFile, SourceOrigin.SourceString s, config,
                                                           FakeHelpers.createParsingOptionsFromFile inFile,
                                                           FakeHelpers.sharedChecker.Value)
        tw.Write(formatted)
    }
    |> Async.RunSynchronously

let private writeInColor consoleColor (content:string) =
    let currentColor = Console.ForegroundColor
    Console.ForegroundColor <- consoleColor
    Console.WriteLine(content)
    Console.ForegroundColor <- currentColor

let [<Literal>] StdInLineLimit = 2000

/// Read input from stdin, with a given line limit until EOF occurs.
///
/// Returns **None** if no lines were read or the stdin was not redirected through a pipe
let readFromStdin (lineLimit:int) =
    // The original functionality of the stdin flag, only accepted redirected input
    if not <| Console.IsInputRedirected then
        None
    else
        let isNotEof = (String.IsNullOrEmpty >> not)
        let input =
            Seq.initInfinite (fun _ -> Console.ReadLine())
            |> Seq.truncate lineLimit
            |> Seq.takeWhile isNotEof
            |> Seq.reduce (+)

        if String.IsNullOrWhiteSpace input then None else Some(input)

[<EntryPoint>]
let main argv =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        let parser = ArgumentParser.Create<Arguments>(programName = "dotnet fantomas", errorHandler = errorHandler)
        let results = parser.ParseCommandLine argv

        let outputPath =
            let hasStdout = results.Contains<@ Arguments.Stdout @>
            if hasStdout then
                OutputPath.StdOut
            else
                match results.TryGetResult<@ Arguments.Out @> with
                | Some output -> OutputPath.IO output
                | None -> OutputPath.Notknown

        let inputPath =
            let maybeInput = results.TryGetResult<@ Arguments.Input @>

            match maybeInput with
            | Some input ->
                if Directory.Exists(input) then
                    InputPath.Folder input
                elif File.Exists input && isFSharpFile input then
                    InputPath.File input
                else
                    InputPath.Unspecified
            | None ->
                let hasStdin = results.Contains<@ Arguments.Stdin @>
                if hasStdin then
                    let stdInInput = readFromStdin StdInLineLimit
                    match stdInInput with
                    | Some input -> InputPath.StdIn input
                    | None -> InputPath.Unspecified
                else
                    InputPath.Unspecified

        let force = results.Contains<@ Arguments.Force @>
        let profile = results.Contains<@ Arguments.Profile @>
        let fsi = results.Contains<@ Arguments.Fsi @>
        let recurse = results.Contains<@ Arguments.Recurse @>
        let version = results.TryGetResult<@ Arguments.Version @>

        let config =
            let defaultConfig =
                results.TryGetResult<@ Arguments.Config @>
                |> Option.map (fun configPath ->
                    let configResult = CodeFormatter.ReadConfiguration configPath
                    match configResult with
                    | Success s -> s
                    | PartialSuccess (ps, warnings) ->
                        List.iter (writeInColor ConsoleColor.DarkYellow) warnings
                        ps
                    | Failure e ->
                        writeInColor ConsoleColor.DarkRed "Couldn't process one or more Fantomas configuration files, falling back to the default configuration"
                        writeInColor ConsoleColor.DarkRed (e.ToString())
                        FormatConfig.Default
                )
                |> Option.defaultValue FormatConfig.Default

            results.GetAllResults()
            |> List.fold (fun acc msg ->
                match msg with
                | Recurse
                | Force
                | Out _
                | Profile
                | Fsi _
                | Stdin _
                | Stdout
                | Config _
                | Version
                | Input _ -> acc
                | Indent i -> { acc with IndentSpaceNum = i }
                | PageWidth pw -> { acc with PageWidth = pw }
                | SemicolonEOL -> { acc with SemicolonAtEndOfLine = true }
                | NoSpaceBeforeArgument -> { acc with SpaceBeforeArgument = false }
                | SpaceBeforeColon -> { acc with SpaceBeforeColon = true }
                | NoSpaceAfterComma -> { acc with SpaceAfterComma = false }
                | NoSpaceAfterSemiColon -> { acc with SpaceAfterSemicolon = false }
                | IndentOnTryWith -> { acc with IndentOnTryWith = true }
                | ReorderOpenDeclaration ->
                    writeInColor ConsoleColor.DarkYellow "Warning: ReorderOpenDeclaration will be removed in the next major version. Using this feature can lead to compilation errors after formatting."
                    { acc with ReorderOpenDeclaration = true }
                | NoSpaceAroundDelimiter -> { acc with SpaceAroundDelimiter = false }
                | KeepNewlineAfter -> { acc with KeepNewlineAfter = true }
                | MaxIfThenElseShortWidth m -> { acc with MaxIfThenElseShortWidth = m }
                | EmptyLineBeforeNestedModuleBody -> { acc with EmptyLineBeforeNestedModuleBody = true }
                | StrictMode -> { acc with StrictMode = true }
            ) defaultConfig

        let fileToFile (inFile : string) (outFile : string) config =
            try
                printfn "Processing %s" inFile
                use buffer = new StreamWriter(outFile)
                if profile then
                    File.ReadLines(inFile) |> Seq.length |> printfn "Line count: %i"
                    time (fun () -> processSourceFile inFile buffer config)
                else
                    processSourceFile inFile buffer config
                buffer.Flush()
                printfn "%s has been written." outFile
            with
            | exn ->
                eprintfn "The following exception occurred while formatting %s: %O" inFile exn
                if force then
                    File.WriteAllText (outFile, File.ReadAllText inFile)
                    printfn "Force writing original contents to %s" outFile

        let stringToFile (s : string) (outFile : string) config =
            try
                let fsi = Path.GetExtension(outFile) = ".fsi"
                if profile then
                    printfn "Line count: %i" (s.Length - s.Replace(Environment.NewLine, "").Length)
                    time (fun () -> processSourceString fsi s (Choice2Of2 outFile) config)
                else
                    processSourceString fsi s (Choice2Of2 outFile) config
                printfn "%s has been written." outFile
            with
            | exn ->
                eprintfn "The following exception occurs while formatting stdin: %O" exn
                if force then
                    File.WriteAllText(outFile, s)
                    printfn "Force writing original contents to %s." outFile

        let stringToStdOut s config =
            try
                use buffer = new StringWriter() :> TextWriter
                processSourceString fsi s (Choice1Of2 buffer) config
                stdout.Write(buffer.ToString())
            with
            | exn ->
                eprintfn "The following exception occurs while formatting stdin: %O" exn
                if force then
                    stdout.Write(s)

        let processFile inputFile outputFile config =
            if inputFile <> outputFile then
                fileToFile inputFile outputFile config
            else
                let content = File.ReadAllText inputFile
                stringToFile content inputFile config

        let processFolder inputFolder outputFolder =
            if not <| Directory.Exists(outputFolder) then
                Directory.CreateDirectory(outputFolder) |> ignore
            allFiles recurse inputFolder
            |> Seq.iter (fun i ->
                // s supposes to have form s1/suffix
                let suffix = i.Substring(inputFolder.Length + 1)
                let o =
                    if inputFolder <> outputFolder then
                        Path.Combine(outputFolder, suffix)
                    else i

                processFile i o config)

        let fileToStdOut inFile config =
            try
                use buffer = new StringWriter()
                // Don't record running time when output formatted content to console
                processSourceFile inFile buffer config
                stdout.Write(buffer.ToString())
            with
            | exn ->
                eprintfn "The following exception occurred while formatting %s: %O" inFile exn
                if force then
                    stdout.Write(File.ReadAllText inFile)

        if Option.isSome version then
            let version = CodeFormatter.GetVersion()
            printfn "Fantomas v%s" version
        else
            match inputPath, outputPath with
            | InputPath.Unspecified, _ ->
                eprintfn "Input path is missing..."
                exit 1
            | InputPath.Folder p1, OutputPath.Notknown -> processFolder p1 p1
            | InputPath.File p1, OutputPath.Notknown -> processFile p1 p1 config
            | InputPath.File p1, OutputPath.IO p2 ->
                processFile p1 p2 config
            | InputPath.Folder p1, OutputPath.IO p2 -> processFolder p1 p2
            | InputPath.StdIn s, OutputPath.IO p ->
                stringToFile s p config
            | InputPath.StdIn s, OutputPath.Notknown
            | InputPath.StdIn s, OutputPath.StdOut ->
                stringToStdOut s config
            | InputPath.File p, OutputPath.StdOut ->
                fileToStdOut p config
            | InputPath.Folder p, OutputPath.StdOut ->
                allFiles recurse p
                |> Seq.iter (fun p -> fileToStdOut p config)
        0