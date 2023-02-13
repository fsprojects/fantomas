open System
open System.IO
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Fantomas.Logging
open Argu
open System.Text
open Spectre.Console

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli" |]

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
                    (Seq.map (fun s -> "*" + s) extensions |> String.concat ",")
            | Verbosity _ -> "Set the verbosity level. Allowed values are n[ormal] and d[etailed]."

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string
    | Unspecified

[<RequireQualifiedAccess>]
type OutputPath =
    | IO of string
    | NotKnown

[<RequireQualifiedAccess>]
type ProcessResult =
    | Formatted of string * ProfileInfos option
    | Ignored of string
    | Unchanged of string * ProfileInfos option
    | Error of string * exn

type Table with

    member x.SetBorder(border: TableBorder) =
        x.Border <- border
        x

let isInExcludedDir (fullPath: string) =
    set [| "obj"; ".fable"; "fable_modules"; "node_modules" |]
    |> Set.map (fun dir -> sprintf "%c%s%c" Path.DirectorySeparatorChar dir Path.DirectorySeparatorChar)
    |> Set.exists fullPath.Contains

let isFSharpFile (s: string) =
    Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, recursively.
let findAllFilesRecursively path =
    let searchOption = SearchOption.AllDirectories

    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let private hasByteOrderMark file =
    async {
        if File.Exists(file) then
            let preamble = Encoding.UTF8.GetPreamble()

            use file = new FileStream(file, FileMode.Open, FileAccess.Read)

            let mutable bom = Array.zeroCreate 3
            do! file.ReadAsync(bom, 0, 3) |> Async.AwaitTask |> Async.Ignore<int>
            return bom = preamble
        else
            return false
    }

/// Format a source string using given config and write to a text writer
let processSourceString (force: bool) (profile: bool) s (fileName: string) config =
    let writeResult (formatted: string) =
        async {
            let! hasBom = hasByteOrderMark fileName

            if hasBom then
                do! File.WriteAllTextAsync(fileName, formatted, Encoding.UTF8) |> Async.AwaitTask
            else
                do! File.WriteAllTextAsync(fileName, formatted) |> Async.AwaitTask

            logGrEqDetailed $"%s{fileName} has been written."
        }

    async {
        let! formatted = s |> Format.formatContentAsync config profile fileName

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent, profileInfos) ->
            do! formattedContent |> writeResult
            return ProcessResult.Formatted(fileName, profileInfos)
        | Format.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! formattedContent |> writeResult
            return ProcessResult.Formatted(fileName, None)
        | Format.FormatResult.Unchanged(file, profileInfos) ->
            logGrEqDetailed $"'%s{file}' was unchanged"
            return ProcessResult.Unchanged(fileName, profileInfos)
        | Format.IgnoredFile file ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return ProcessResult.Ignored fileName
        | Format.FormatResult.Error(file, ex) -> return ProcessResult.Error(file, ex)
        | Format.InvalidCode(file, _) ->
            let ex = FormatException($"Formatting {file} lead to invalid F# code")
            return ProcessResult.Error(file, ex)
    }

/// Format inFile and write to text writer
let processSourceFile (force: bool) (profile: bool) inFile (tw: TextWriter) =
    async {
        let! formatted = Format.formatFileAsync profile inFile

        match formatted with
        | Format.FormatResult.Formatted(_, formattedContent, profileInfos) ->
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return ProcessResult.Formatted(inFile, profileInfos)
        | Format.InvalidCode(file, formattedContent) when force ->
            stdlog $"%s{file} was not valid after formatting."
            do! tw.WriteAsync(formattedContent) |> Async.AwaitTask
            return ProcessResult.Formatted(inFile, None)
        | Format.FormatResult.Unchanged(_, profileInfos) ->
            let! input = inFile |> File.ReadAllTextAsync |> Async.AwaitTask
            do! input |> tw.WriteAsync |> Async.AwaitTask
            return ProcessResult.Unchanged(inFile, profileInfos)
        | Format.IgnoredFile file ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return ProcessResult.Ignored inFile
        | Format.FormatResult.Error(file, ex) -> return ProcessResult.Error(file, ex)
        | Format.InvalidCode(file, _) ->
            let ex = FormatException($"Formatting {file} lead to invalid F# code")
            return ProcessResult.Error(file, ex)
    }

let private reportCheckResults (checkResult: Format.CheckResult) =
    checkResult.Errors
    |> List.map (fun (filename, exn) -> $"error: Failed to format %s{filename}: %s{exn.ToString()}")
    |> Seq.iter elog

    checkResult.Formatted
    |> List.map (fun filename -> $"%s{filename} needs formatting")
    |> Seq.iter stdlog

let runCheckCommand (inputPath: InputPath) : int =
    let check files =
        Async.RunSynchronously(Format.checkCode files)

    let processCheckResult (checkResult: Format.CheckResult) =
        if checkResult.IsValid then
            logGrEqDetailed "No changes required."
            0
        else
            reportCheckResults checkResult
            if checkResult.HasErrors then 1 else 99

    match inputPath with
    | InputPath.NoFSharpFile s ->
        elog $"Input path '%s{s}' is unsupported file type"
        1
    | InputPath.NotFound s ->
        elog $"Input path '%s{s}' not found"
        1
    | InputPath.Unspecified _ ->
        elog "No input path provided. Call with --help for usage information."
        1
    | InputPath.File f when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
        logGrEqDetailed $"'%s{f}' was ignored"
        0
    | InputPath.File path -> path |> Seq.singleton |> check |> processCheckResult
    | InputPath.Folder path -> path |> findAllFilesRecursively |> check |> processCheckResult
    | InputPath.Multiple(files, folders) ->
        let allFilesToCheck =
            seq {
                yield! files
                yield! (Seq.collect findAllFilesRecursively folders)
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
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let inputPath =
        let maybeInput = results.TryGetResult <@ Arguments.Input @>

        match maybeInput with
        | Some [ input ] ->
            if Directory.Exists(input) then
                InputPath.Folder input
            elif File.Exists input && isFSharpFile input then
                InputPath.File input
            elif File.Exists input then
                InputPath.NoFSharpFile input
            else
                InputPath.NotFound input
        | Some inputs ->
            let isFolder (path: string) = Path.GetExtension(path) = ""

            let rec loop
                (files: string list)
                (finalContinuation: string list * string list -> string list * string list)
                =
                match files with
                | [] -> finalContinuation ([], [])
                | h :: rest ->
                    loop rest (fun (files, folders) ->
                        if isFolder h then
                            files, (h :: folders)
                        else
                            (h :: files), folders
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

    let fileToFile (force: bool) (inFile: string) (outFile: string) =
        async {
            logGrEqDetailed $"Processing %s{inFile}"
            let! hasByteOrderMark = hasByteOrderMark inFile

            use buffer =
                if hasByteOrderMark then
                    new StreamWriter(
                        new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new StreamWriter(outFile)

            let! processResult = processSourceFile force profile inFile buffer

            do! buffer.FlushAsync() |> Async.AwaitTask
            logGrEqDetailed $"%s{outFile} has been written."
            return processResult
        }

    let stringToFile (force: bool) (s: string) (outFile: string) config =
        async { return! processSourceString force profile s outFile config }

    let processFile force inputFile outputFile =
        async {
            try
                if inputFile <> outputFile then
                    return! fileToFile force inputFile outputFile
                else
                    logGrEqDetailed $"Processing %s{inputFile}"
                    let! content = File.ReadAllTextAsync inputFile |> Async.AwaitTask
                    let config = EditorConfig.readConfiguration inputFile
                    return! stringToFile force content inputFile config
            with e ->
                return ProcessResult.Error(inputFile, e)
        }

    let processFolder force inputFolder outputFolder =
        if not <| Directory.Exists(outputFolder) then
            Directory.CreateDirectory(outputFolder) |> ignore

        findAllFilesRecursively inputFolder
        |> Seq.toList
        |> List.map (fun i ->
            // s supposes to have form s1/suffix
            let suffix = i.Substring(inputFolder.Length + 1)

            let o =
                if inputFolder <> outputFolder then
                    Path.Combine(outputFolder, suffix)
                else
                    i

            processFile force i o)

    let filesAndFolders force (files: string list) (folders: string list) =
        let fileTasks =
            files
            |> List.map (fun file ->
                if (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file) then
                    logGrEqDetailed $"'%s{file}' was ignored"
                    async.Return(ProcessResult.Ignored(file))
                else
                    processFile force file file)

        let folderTasks =
            folders |> List.collect (fun folder -> processFolder force folder folder)

        (fileTasks @ folderTasks)

    let check = results.Contains <@ Arguments.Check @>
    let isDaemon = results.Contains <@ Arguments.Daemon @>

    let partitionResults (results: #seq<ProcessResult>) =
        (([], [], [], []), results)
        ||> Seq.fold (fun (oks, ignores, unchanged, errors) next ->
            match next with
            | ProcessResult.Formatted(x, _) -> (x :: oks, ignores, unchanged, errors)
            | ProcessResult.Ignored i -> (oks, i :: ignores, unchanged, errors)
            | ProcessResult.Unchanged(u, _) -> (oks, ignores, u :: unchanged, errors)
            | ProcessResult.Error(file, e) -> (oks, ignores, unchanged, (file, e) :: errors))

    let reportFormatResults (results: #seq<ProcessResult>) =
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

        let reportProfileInfos results =
            if profile && not (Seq.isEmpty results) then
                let profileInfos =
                    seq {
                        for r in results do
                            match r with
                            | ProcessResult.Formatted(f, Some p) -> yield (f, p)
                            | ProcessResult.Unchanged(f, Some p) -> yield (f, p)
                            | _ -> ()
                    }

                let table = Table().AddColumns([| "File"; "Line count"; "Time taken" |])

                profileInfos
                |> Seq.fold (fun (t: Table) (f, p) -> t.AddRow([| f; string p.LineCount; string p.TimeTaken |])) table
                |> AnsiConsole.Write

        match Seq.tryExactlyOne results with
        | Some singleResult ->
            let fileName f = FileInfo(f).Name

            let reportProfileInfo (f, p) =
                match profile, p with
                | true, Some pI -> stdlog $"%s{f} Line count: %d{pI.LineCount} Time taken {pI.TimeTaken}"
                | _ -> ()

            match singleResult with
            | ProcessResult.Formatted(f, p) ->
                stdlog $"{fileName f} was formatted."
                reportProfileInfo (f, p)
            | ProcessResult.Ignored f -> stdlog $"{fileName f} was ignored."
            | ProcessResult.Unchanged(f, p) ->
                stdlog $"{fileName f} was unchanged."
                reportProfileInfo (f, p)
            | ProcessResult.Error(f, e) ->
                reportError (fileName f, e)
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

            reportProfileInfos results

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
