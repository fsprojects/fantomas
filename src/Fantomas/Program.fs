open System
open System.IO.Abstractions
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Fantomas.Logging
open Argu
open System.Text
open Spectre.Console

type Arguments =
    | [<Unique>] Force
    | [<Unique>] Profile
    | [<Unique>] Out of string
    | [<Unique>] Check
    | [<Unique>] Daemon
    | [<Unique>] Version
    | [<Unique; AltCommandLine("-v")>] Verbosity of string
    | [<MainCommand>] Input of string list

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Force -> "Print the output even if it is not valid F# code. For debugging purposes only."
            | Out _ ->
                "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only. Multiple files/folders are not supported."
            | Profile -> "Print performance profiling information."
            | Check ->
                "Don't format files, just check if they have changed. Exits with 0 if it's formatted correctly, with 1 if some files need formatting and 99 if there was an internal error"
            | Daemon -> "Daemon mode, launches an LSP-like server to can be used by editor tooling."
            | Version -> "Displays the version of Fantomas"
            | Input _ ->
                sprintf
                    "Input paths: can be multiple folders or files with %s extension."
                    (Seq.map (fun s -> "*" + s) InputFiles.acceptedFSharpExtensions
                     |> String.concat ",")
            | Verbosity _ -> "Set the verbosity level. Allowed values are n[ormal] and d[etailed]."

[<RequireQualifiedAccess>]
type InputPath =
    | File of IFileInfo
    | Folder of IDirectoryInfo
    | Multiple of files: IFileInfo list * folder: IDirectoryInfo list
    | NoFSharpFile of IFileInfo
    | NotFound of string
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | NotKnown

type Table with

    member x.SetBorder(border: TableBorder) =
        x.Border <- border
        x

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let hasByteOrderMark (fs: IFileSystem) file =
    async {
        if fs.File.Exists(file) then
            let preamble = Encoding.UTF8.GetPreamble()

            use file =
                fs.FileStream.Create(file, System.IO.FileMode.Open, System.IO.FileAccess.Read)

            let mutable bom = Array.zeroCreate 3
            do! file.ReadAsync(bom, 0, 3) |> Async.AwaitTask |> Async.Ignore<int>
            return bom = preamble
        else
            return false
    }

let invalidResultException file =
    FormatException($"Formatting {file} leads to invalid F# code")

/// Format a source string using given config and write to a text writer
let processSourceString (fs: IFileSystem) (force: bool) (profile: bool) s (fileName: string) config =
    let writeResult (formatted: string) =
        async {
            let! hasBom = hasByteOrderMark fs fileName

            if hasBom then
                do! fs.File.WriteAllTextAsync(fileName, formatted, Encoding.UTF8) |> Async.AwaitTask
            else
                do! fs.File.WriteAllTextAsync(fileName, formatted) |> Async.AwaitTask

            logGrEqDetailed $"%s{fileName} has been written."
        }

    async {
        let formatParams = FormatParams.Create(config, false, profile, fileName)
        let! formatted = s |> Format.formatContentAsync formatParams

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! formattedContent |> writeResult
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! formattedContent |> writeResult
            return FormatResult.Formatted(fileName, formattedContent, None)
        | FormatResult.Unchanged(file, _) as r ->
            logGrEqDetailed $"'%s{file}' was unchanged"
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = invalidResultException file
            return FormatResult.Error(file, ex)
    }

/// Format inFile and write to text writer
let processSourceFile (fs: IFileSystem) (force: bool) (profile: bool) inFile (tw: System.IO.TextWriter) =
    async {
        let! formatted = FormatParams.Create(false, profile, inFile) |> Format.formatFileAsync

        match formatted with
        | FormatResult.Formatted(_, formattedContent, _) as r ->
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return r
        | FormatResult.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return FormatResult.Formatted(inFile, formattedContent, None)
        | FormatResult.Unchanged _ as r ->
            let! input = inFile |> fs.File.ReadAllTextAsync |> Async.AwaitTask
            do! input |> tw.WriteAsync |> Async.AwaitTask
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = invalidResultException file
            return FormatResult.Error(file, ex)
    }

let reportCheckResults (checkResult: CheckResult) =
    checkResult.Errors
    |> List.map (fun (filename, exn) -> $"error: Failed to format %s{filename}: %s{exn.ToString()}")
    |> Seq.iter elog

    checkResult.Formatted
    |> List.map (fun filename -> $"%s{filename} needs formatting")
    |> Seq.iter stdlog

let runCheckCommand (ignoreFile: IgnoreFile option) (inputPath: InputPath) : int =
    let check files =
        Async.RunSynchronously(Format.checkCode files)

    let processCheckResult (checkResult: CheckResult) =
        if checkResult.IsValid then
            logGrEqDetailed "No changes required."
            0
        else
            reportCheckResults checkResult
            if checkResult.HasErrors then 1 else 99

    match inputPath with
    | InputPath.NoFSharpFile file ->
        elog $"Input path '%s{file.FullName}' is unsupported file type"
        1
    | InputPath.NotFound file ->
        elog $"Input path '%s{file}' not found"
        1
    | InputPath.Unspecified _ ->
        elog "No input path provided. Call with --help for usage information."
        1
    | InputPath.File f when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
        logGrEqDetailed $"'%s{f.FullName}' was ignored"
        0
    | InputPath.File path -> path |> Seq.singleton |> check |> processCheckResult
    | InputPath.Folder path -> path |> InputFiles.getFilesForFolder ignoreFile |> check |> processCheckResult
    | InputPath.Multiple(files, folders) ->
        let allFilesToCheck =
            seq {
                yield! files
                yield! (Seq.collect (InputFiles.getFilesForFolder ignoreFile) folders)
            }

        allFilesToCheck |> check |> processCheckResult

let mainAux (fs: IFileSystem) argv =
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

    let ignoreFile = IgnoreFile.current.Force()

    let outputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let inputPath =
        let maybeInput = results.TryGetResult <@ Arguments.Input @>

        match maybeInput with
        | Some [ input ] ->
            if fs.Directory.Exists(input) then
                InputPath.Folder(fs.DirectoryInfo.FromDirectoryName(input))
            elif not (fs.File.Exists(input)) then
                InputPath.NotFound input
            else
                let file = fs.FileInfo.FromFileName input

                if not (InputFiles.isFSharpFile file) then
                    InputPath.NoFSharpFile file
                else
                    InputPath.File file

        | Some inputs ->
            let isFolder (path: string) = fs.Path.GetExtension(path) = ""

            let rec loop
                (files: string list)
                (finalContinuation: IFileInfo list * IDirectoryInfo list -> IFileInfo list * IDirectoryInfo list)
                =
                match files with
                | [] -> finalContinuation ([], [])
                | h :: rest ->
                    loop rest (fun (files, folders) ->
                        if isFolder h then
                            files, (fs.DirectoryInfo.FromDirectoryName h :: folders)
                        else
                            (fs.FileInfo.FromFileName h :: files), folders
                        |> finalContinuation)

            let filesAndFolders = loop inputs id
            InputPath.Multiple filesAndFolders
        | None -> InputPath.Unspecified

    let force = results.Contains <@ Arguments.Force @>
    let profile = results.Contains <@ Arguments.Profile @>
    let version = results.TryGetResult <@ Arguments.Version @>

    let maybeVerbosity =
        results.TryGetResult <@ Arguments.Verbosity @>
        |> Option.map (fun v -> v.ToLowerInvariant())

    let verbosity =
        match maybeVerbosity with
        | None
        | Some "n"
        | Some "normal" -> initLogger VerbosityLevel.Normal
        | Some "d"
        | Some "detailed" -> initLogger VerbosityLevel.Detailed
        | Some _ ->
            elog "Invalid verbosity level"
            exit 1

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    let fileToFile (fs: IFileSystem) (force: bool) (inFile: string) (outFile: string) =
        async {
            logGrEqDetailed $"Processing %s{inFile}"
            let! hasByteOrderMark = hasByteOrderMark fs inFile

            use buffer =
                if hasByteOrderMark then
                    new System.IO.StreamWriter(
                        fs.FileStream.Create(outFile, System.IO.FileMode.OpenOrCreate, System.IO.FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new System.IO.StreamWriter(outFile)

            let! processResult = processSourceFile fs force profile inFile buffer

            do! buffer.FlushAsync() |> Async.AwaitTask
            logGrEqDetailed $"%s{outFile} has been written."
            return processResult
        }

    let stringToFile (fs: IFileSystem) (force: bool) (s: string) (outFile: string) config =
        async { return! processSourceString fs force profile s outFile config }

    let processFile (fs: IFileSystem) force (inputFile: IFileInfo) (outputFile: IFileInfo) =
        async {
            try
                if inputFile <> outputFile then
                    return! fileToFile fs force inputFile outputFile
                else
                    logGrEqDetailed $"Processing %s{inputFile}"
                    let! content = fs.File.ReadAllTextAsync inputFile |> Async.AwaitTask
                    let config = EditorConfig.readConfiguration inputFile
                    return! stringToFile fs force content inputFile config
            with e ->
                return FormatResult.Error(inputFile, e)
        }

    let processFolder
        (fs: IFileSystem)
        (ignoreFile: IgnoreFile option)
        force
        (inputFolder: IDirectoryInfo)
        (outputFolder: IDirectoryInfo)
        =
        if not outputFolder.Exists then
            outputFolder.Create()

        InputFiles.getFilesForFolder ignoreFile inputFolder
        |> Seq.toList
        |> List.map (fun (file: IFileInfo) ->
            // s supposes to have form s1/suffix
            let suffix = file.FullName.Substring(inputFolder.FullName.Length + 1)

            let outputFile =
                if inputFolder <> outputFolder then
                    fs.Path.Combine(outputFolder.FullName, suffix) |> fs.FileInfo.FromFileName
                else
                    file

            processFile fs force file outputFile)

    let filesAndFolders
        (fs: IFileSystem)
        (ignoreFile: IgnoreFile option)
        force
        (files: IFileInfo list)
        (folders: IDirectoryInfo list)
        =
        [ for file in files do
              yield processFile fs force file file

          for folder in folders do
              yield! processFolder fs ignoreFile force folder folder ]

    let check = results.Contains <@ Arguments.Check @>
    let isDaemon = results.Contains <@ Arguments.Daemon @>

    let partitionResults (results: #seq<FormatResult>) =
        (([], [], [], []), results)
        ||> Seq.fold (fun (oks, ignores, unchanged, errors) next ->
            match next with
            | FormatResult.Formatted(file, _, p) -> ((file, p) :: oks, ignores, unchanged, errors)
            | FormatResult.Unchanged(file, p) -> (oks, ignores, (file, p) :: unchanged, errors)
            | FormatResult.Error(file, e) -> (oks, ignores, unchanged, (file, e) :: errors)
            | FormatResult.InvalidCode(file, _) ->
                let ex = invalidResultException file
                (oks, ignores, unchanged, (file, ex) :: errors))

    let reportFormatResults (results: #seq<FormatResult>) =
        let reportError (file, exn: Exception) =
            let message =
                match verbosity with
                | VerbosityLevel.Normal ->
                    match exn with
                    | :? ParseException -> "Could not parse file."
                    | :? FormatException as fe -> fe.Message
                    | _ -> ""
                | VerbosityLevel.Detailed -> $"%A{exn}"

            let message =
                if String.IsNullOrEmpty message then
                    message
                else
                    $" : {message}"

            elog $"Failed to format file: {file}{message}"

        let reportProfileInfos (results: (string * ProfileInfo option) list) =
            if profile && not (List.isEmpty results) then
                let table = Table().AddColumns([| "File"; "Line count"; "Time taken" |])

                results
                |> List.choose (fun (f, p) -> p |> Option.map (fun p -> f, p))
                |> List.sortBy fst
                |> List.fold
                    (fun (t: Table) (f, p) ->
                        t.AddRow([| f; string p.LineCount; p.TimeTaken.ToString("mm\:ss\.fff") |]))
                    table
                |> AnsiConsole.Write

        match Seq.tryExactlyOne results with
        | Some singleResult ->
            let fileName f = FileInfo(f).Name

            let reportProfileInfo (f, p: ProfileInfo option) =
                match profile, p with
                | true, Some pI -> stdlog $"%s{f} Line count: %d{pI.LineCount} Time taken {pI.TimeTaken}"
                | _ -> ()

            match singleResult with
            | FormatResult.Formatted(f, _, p) ->
                stdlog $"{fileName f} was formatted."
                reportProfileInfo (f, p)
            | FormatResult.Unchanged(f, p) ->
                stdlog $"{fileName f} was unchanged."
                reportProfileInfo (f, p)
            | FormatResult.Error(f, e) ->
                reportError (fileName f, e)
                exit 1
            | FormatResult.InvalidCode(f, _) ->
                let ex = invalidResultException f
                reportError (fileName f, ex)
                exit 1

        | None ->
            let oks, ignored, unchanged, errored = partitionResults results
            let centeredColumn (v: string) = TableColumn(v).Centered()

            Table()
                .AddColumns(
                    [| "[green]Formatted[/]"
                       string oks.Length
                       "Ignored"
                       string ignored.Length
                       "[blue]Unchanged[/]"
                       string unchanged.Length
                       "[red]Errored[/]"
                       string errored.Length |]
                    |> Array.map centeredColumn
                )
                .SetBorder(TableBorder.MinimalDoubleHead)
            |> AnsiConsole.Write

            for e in errored do
                reportError e

            reportProfileInfos (oks @ unchanged)

            if errored.Length > 0 then
                exit 1

    let asyncRunner = Async.Parallel >> Async.RunSynchronously

    if Option.isSome version then
        let version = CodeFormatter.GetVersion()
        stdlog $"Fantomas v%s{version}"
    elif isDaemon then
        let daemon =
            new FantomasDaemon(Console.OpenStandardOutput(), Console.OpenStandardInput())

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        exit 0
    elif check then
        inputPath |> runCheckCommand |> exit
    else
        try
            match inputPath, outputPath with
            | InputPath.NoFSharpFile s, _ ->
                elog $"Input path '%s{s}' is unsupported file type."
                exit 1
            | InputPath.NotFound s, _ ->
                elog $"Input path '%s{s}' not found."
                exit 1
            | InputPath.Unspecified, _ ->
                elog "Input path is missing. Call with --help for usage information."
                exit 1
            | InputPath.File f, _ when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
                logGrEqDetailed $"'%s{f}' was ignored"
            | InputPath.Folder p1, OutputPath.NotKnown ->
                processFolder force p1 p1 |> asyncRunner |> reportFormatResults
            | InputPath.File p1, OutputPath.NotKnown ->
                processFile force p1 p1 |> List.singleton |> asyncRunner |> reportFormatResults
            | InputPath.File p1, OutputPath.IO p2 ->
                processFile force p1 p2 |> List.singleton |> asyncRunner |> reportFormatResults
            | InputPath.Folder p1, OutputPath.IO p2 -> processFolder force p1 p2 |> asyncRunner |> reportFormatResults
            | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
                filesAndFolders force files folders |> asyncRunner |> reportFormatResults
            | InputPath.Multiple _, OutputPath.IO _ ->
                elog "Multiple input files are not supported with the --out flag."
                exit 1
        with exn ->
            elog $"%s{exn.Message}"
            exit 1

    0

[<EntryPoint>]
let main argv = mainAux (FileSystem()) argv
