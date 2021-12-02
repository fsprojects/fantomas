open System
open System.IO
open Fantomas
open Fantomas.FormatConfig
open Fantomas.Extras
open Argu
open System.Text

let extensions =
    set [| ".fs"
           ".fsx"
           ".fsi"
           ".ml"
           ".mli" |]

type Arguments =
    | [<Unique; AltCommandLine("-r")>] Recurse
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Fsi of string
    | [<Unique>] Stdin
    | [<Unique>] Stdout
    | [<Unique>] Out of string
    | [<Unique>] Check
    | [<Unique; AltCommandLine("-v")>] Version
    | [<MainCommand>] Input of string list
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Recurse -> "Process the input folder recursively."
            | Force -> "Print the source unchanged if it cannot be parsed correctly."
            | Out _ ->
                "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only."
            | Profile -> "Print performance profiling information."
            | Fsi _ -> "Read F# source from stdin as F# signatures."
            | Stdin -> "Read F# source from standard input."
            | Stdout -> "Write the formatted source code to standard output."
            | Check ->
                "Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error"
            | Version -> "Displays the version of Fantomas"
            | Input _ ->
                sprintf
                    "Input paths: can be multiple folders or files with %s extension."
                    (Seq.map (fun s -> "*" + s) extensions
                     |> String.concat ",")

let time f =
    let sw = Diagnostics.Stopwatch.StartNew()
    let res = f ()
    sw.Stop()
    printfn "Time taken: %O s" sw.Elapsed
    res

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | StdIn of string
    | Multiple of files: string list * folder: string list
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | StdOut
    | NotKnown

let isInExcludedDir (fullPath: string) =
    set [| "obj"
           ".fable"
           "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) =
    Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    let searchOption =
        (if isRec then
             SearchOption.AllDirectories
         else
             SearchOption.TopDirectoryOnly)

    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter
        (fun f ->
            isFSharpFile f
            && not (isInExcludedDir f)
            && not (IgnoreFile.isIgnoredFile f))

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let private hasByteOrderMark file =
    if File.Exists(file) then
        let preamble = Encoding.UTF8.GetPreamble()

        use file =
            new FileStream(file, FileMode.Open, FileAccess.Read)

        let mutable bom = Array.zeroCreate 3
        file.Read(bom, 0, 3) |> ignore
        bom = preamble
    else
        false

/// Format a source string using given config and write to a text writer
let processSourceString isFsiFile s (tw: Choice<TextWriter, string>) config =
    let fileName =
        match tw, isFsiFile with
        | Choice1Of2 _, isFsi ->
            let extension = if isFsi then "fsi" else "fs"
            sprintf "/tmp.%s" extension
        | Choice2Of2 f, _ -> f

    let writeResult (formatted: string) =
        match tw with
        | Choice1Of2 tw -> tw.Write(formatted)
        | Choice2Of2 path ->
            if hasByteOrderMark path then
                File.WriteAllText(path, formatted, Encoding.UTF8)
            else
                File.WriteAllText(path, formatted)

            printfn "%s has been written." path

    async {
        let! formatted =
            s
            |> FakeHelpers.formatContentAsync config fileName

        match formatted with
        | FakeHelpers.FormatResult.Formatted (_, formattedContent) -> formattedContent |> writeResult
        | FakeHelpers.FormatResult.Unchanged file -> printfn "'%s' was unchanged" file
        | FakeHelpers.IgnoredFile file -> printfn "'%s' was ignored" file
        | FakeHelpers.FormatResult.Error (_, ex) -> raise <| ex
    }
    |> Async.RunSynchronously

/// Format inFile and write to text writer
let processSourceFile inFile (tw: TextWriter) =
    async {
        let! formatted = FakeHelpers.formatFileAsync inFile

        match formatted with
        | FakeHelpers.FormatResult.Formatted (_, formattedContent) -> tw.Write(formattedContent)
        | FakeHelpers.FormatResult.Unchanged _ -> inFile |> File.ReadAllText |> tw.Write
        | FakeHelpers.IgnoredFile file -> printfn "'%s' was ignored" file
        | FakeHelpers.FormatResult.Error (_, ex) -> raise <| ex
    }
    |> Async.RunSynchronously

let private writeInColor consoleColor (content: string) =
    let currentColor = Console.ForegroundColor
    Console.ForegroundColor <- consoleColor
    Console.WriteLine(content)
    Console.ForegroundColor <- currentColor

[<Literal>]
let StdInLineLimit = 2000

/// Read input from stdin, with a given line limit until EOF occurs.
///
/// Returns **None** if no lines were read or the stdin was not redirected through a pipe
let readFromStdin (lineLimit: int) =
    // The original functionality of the stdin flag, only accepted redirected input
    if not <| Console.IsInputRedirected then
        None
    else
        let isNotEof line = not <| isNull line
        let appendWithNewline acc next = acc + "\n" + next

        let input =
            Seq.initInfinite (fun _ -> Console.ReadLine())
            |> Seq.truncate lineLimit
            |> Seq.takeWhile isNotEof
            |> Seq.reduce appendWithNewline

        if String.IsNullOrWhiteSpace input then
            None
        else
            Some(input)

let private reportCheckResults (output: TextWriter) (checkResult: FakeHelpers.CheckResult) =
    checkResult.Errors
    |> List.map (fun (filename, exn) -> sprintf "error: Failed to format %s: %s" filename (exn.ToString()))
    |> Seq.iter output.WriteLine

    checkResult.Formatted
    |> List.map (sprintf "%s needs formatting")
    |> Seq.iter output.WriteLine

let runCheckCommand (recurse: bool) (inputPath: InputPath) : int =
    let check files =
        Async.RunSynchronously(FakeHelpers.checkCode files)

    let processCheckResult (checkResult: FakeHelpers.CheckResult) =
        if checkResult.IsValid then
            stdout.WriteLine "No changes required."
            0
        else
            reportCheckResults stdout checkResult
            if checkResult.HasErrors then 1 else 99

    match inputPath with
    | InputPath.Unspecified
    | InputPath.StdIn _ ->
        eprintfn "No input path provided. Nothing to do."
        0
    | InputPath.File f when (IgnoreFile.isIgnoredFile f) ->
        printfn "'%s' was ignored" f
        0
    | InputPath.File path ->
        path
        |> Seq.singleton
        |> check
        |> processCheckResult
    | InputPath.Folder path ->
        path
        |> allFiles recurse
        |> check
        |> processCheckResult
    | InputPath.Multiple (files, folders) ->
        let allFilesToCheck =
            seq {
                yield! files
                yield! (Seq.collect (allFiles recurse) folders)
            }

        allFilesToCheck |> check |> processCheckResult

[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = "dotnet fantomas", errorHandler = errorHandler)

    let results = parser.ParseCommandLine argv

    let outputPath =
        let hasStdout = results.Contains <@ Arguments.Stdout @>

        if hasStdout then
            OutputPath.StdOut
        else
            match results.TryGetResult <@ Arguments.Out @> with
            | Some output -> OutputPath.IO output
            | None -> OutputPath.NotKnown

    let inputPath =
        let maybeInput =
            results.TryGetResult <@ Arguments.Input @>

        match maybeInput with
        | Some [ input ] ->
            if Directory.Exists(input) then
                InputPath.Folder input
            elif File.Exists input && isFSharpFile input then
                InputPath.File input
            else
                InputPath.Unspecified
        | Some inputs ->
            let isFolder (path: string) = Path.GetExtension(path) = ""

            let rec loop
                (files: string list)
                (finalContinuation: string list * string list -> string list * string list)
                =
                match files with
                | [] -> finalContinuation ([], [])
                | h :: rest ->
                    loop
                        rest
                        (fun (files, folders) ->
                            if isFolder h then
                                files, (h :: folders)
                            else
                                (h :: files), folders
                            |> finalContinuation)

            let filesAndFolders = loop inputs id
            InputPath.Multiple filesAndFolders
        | None ->
            let hasStdin = results.Contains <@ Arguments.Stdin @>

            if hasStdin then
                let stdInInput = readFromStdin StdInLineLimit

                match stdInInput with
                | Some input -> InputPath.StdIn input
                | None -> InputPath.Unspecified
            else
                InputPath.Unspecified

    let force = results.Contains <@ Arguments.Force @>
    let profile = results.Contains <@ Arguments.Profile @>
    let fsi = results.Contains <@ Arguments.Fsi @>
    let recurse = results.Contains <@ Arguments.Recurse @>

    let version =
        results.TryGetResult <@ Arguments.Version @>

    let fileToFile (inFile: string) (outFile: string) =
        try
            printfn "Processing %s" inFile
            let hasByteOrderMark = hasByteOrderMark inFile

            use buffer =
                if hasByteOrderMark then
                    new StreamWriter(
                        new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new StreamWriter(outFile)

            if profile then
                File.ReadLines(inFile)
                |> Seq.length
                |> printfn "Line count: %i"

                time (fun () -> processSourceFile inFile buffer)
            else
                processSourceFile inFile buffer

            buffer.Flush()
            printfn "%s has been written." outFile
        with
        | exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn

            if force then
                File.WriteAllText(outFile, File.ReadAllText inFile)
                printfn "Force writing original contents to %s" outFile

            reraise ()

    let stringToFile (s: string) (outFile: string) config =
        try
            let fsi = Path.GetExtension(outFile) = ".fsi"

            if profile then
                printfn
                    "Line count: %i"
                    (s.Length
                     - s.Replace(Environment.NewLine, "").Length)

                time (fun () -> processSourceString fsi s (Choice2Of2 outFile) config)
            else
                processSourceString fsi s (Choice2Of2 outFile) config
        with
        | exn ->
            eprintfn "The following exception occurs while formatting stdin: %O" exn

            if force then
                File.WriteAllText(outFile, s)
                printfn "Force writing original contents to %s." outFile

            reraise ()

    let stringToStdOut s config =
        try
            use buffer = new StringWriter() :> TextWriter
            processSourceString fsi s (Choice1Of2 buffer) config
            stdout.Write(buffer.ToString())
        with
        | exn ->
            eprintfn "The following exception occurs while formatting stdin: %O" exn
            if force then stdout.Write(s)
            reraise ()

    let processFile inputFile outputFile =
        if inputFile <> outputFile then
            fileToFile inputFile outputFile
        else
            printfn "Processing %s" inputFile
            let content = File.ReadAllText inputFile
            let config = EditorConfig.readConfiguration inputFile
            stringToFile content inputFile config

    let processFolder inputFolder outputFolder =
        if not <| Directory.Exists(outputFolder) then
            Directory.CreateDirectory(outputFolder) |> ignore

        allFiles recurse inputFolder
        |> Seq.iter
            (fun i ->
                // s supposes to have form s1/suffix
                let suffix = i.Substring(inputFolder.Length + 1)

                let o =
                    if inputFolder <> outputFolder then
                        Path.Combine(outputFolder, suffix)
                    else
                        i

                processFile i o)

    let fileToStdOut inFile =
        try
            use buffer = new StringWriter()
            // Don't record running time when output formatted content to console
            processSourceFile inFile buffer
            stdout.Write(buffer.ToString())
        with
        | exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn

            if force then
                stdout.Write(File.ReadAllText inFile)

            reraise ()

    let filesAndFolders (files: string list) (folders: string list) : unit =
        files
        |> List.iter
            (fun file ->
                if (IgnoreFile.isIgnoredFile file) then
                    printfn "'%s' was ignored" file
                else
                    processFile file file)

        folders
        |> List.iter (fun folder -> processFolder folder folder)

    let check = results.Contains <@ Arguments.Check @>

    if Option.isSome version then
        let version = CodeFormatter.GetVersion()
        printfn "Fantomas v%s" version
    elif check then
        inputPath |> runCheckCommand recurse |> exit
    else
        try
            match inputPath, outputPath with
            | InputPath.Unspecified, _ ->
                eprintfn "Input path is missing..."
                exit 1
            | InputPath.File f, _ when (IgnoreFile.isIgnoredFile f) -> printfn "'%s' was ignored" f
            | InputPath.Folder p1, OutputPath.NotKnown -> processFolder p1 p1
            | InputPath.File p1, OutputPath.NotKnown -> processFile p1 p1
            | InputPath.File p1, OutputPath.IO p2 -> processFile p1 p2
            | InputPath.Folder p1, OutputPath.IO p2 -> processFolder p1 p2
            | InputPath.StdIn s, OutputPath.IO p -> stringToFile s p FormatConfig.Default
            | InputPath.StdIn s, OutputPath.NotKnown
            | InputPath.StdIn s, OutputPath.StdOut -> stringToStdOut s FormatConfig.Default
            | InputPath.File p, OutputPath.StdOut -> fileToStdOut p
            | InputPath.Folder p, OutputPath.StdOut -> allFiles recurse p |> Seq.iter fileToStdOut
            | InputPath.Multiple _,
              (OutputPath.StdOut
              | OutputPath.IO _) ->
                eprintfn "--stdout and --out cannot be combined with multiple files."
                exit 1
            | InputPath.Multiple (files, folders), OutputPath.NotKnown -> filesAndFolders files folders
        with
        | exn ->
            printfn "%s" exn.Message
            exit 1

    0
