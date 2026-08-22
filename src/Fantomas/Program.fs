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

[<HelpFlags("--help", "-h")>]
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
                "Report which files need formatting and write nothing. Exits with 0 when every file is already formatted, with 99 when some file needs formatting, and with 1 when an error occurred."
            | Daemon -> "Daemon mode, launches an LSP-like server that can be used by editor tooling."
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

/// Every F# file below the given path, at any depth. Build output and package folders are
/// skipped: formatting what a compiler or a package manager wrote is never what was asked for.
let findAllFilesRecursively path =
    let searchOption = SearchOption.AllDirectories

    Directory.GetFiles(path, "*.*", searchOption)
    |> Seq.filter (fun f -> isFSharpFile f && not (isInExcludedDir f))

/// Create the folders leading up to a file, so that writing to a path the user named but never
/// created succeeds. Path.GetDirectoryName yields an empty string for a bare file name.
let ensureParentFolderExists (file: string) : unit =
    let folder = Path.GetDirectoryName(file)

    if not (String.IsNullOrEmpty folder) then
        Directory.CreateDirectory(folder) |> ignore

/// Do two paths name the same location? `src` and `./src` do, and comparing them as they were
/// typed does not say so. This is about spelling, not about the file system: a path reached
/// through a symbolic link, or through a spelling a case insensitive volume accepts, is not
/// recognised here. Nothing may depend on a negative answer to avoid destroying a file.
let isSamePath (left: string) (right: string) : bool =
    String.Equals(Path.GetFullPath left, Path.GetFullPath right, StringComparison.Ordinal)

/// Is a file located inside a folder, at any depth?
let isInFolder (folder: string) (file: string) : bool =
    let folder =
        String.Concat(
            Path.GetFullPath(folder).TrimEnd(Path.DirectorySeparatorChar),
            string<char> Path.DirectorySeparatorChar
        )

    Path.GetFullPath(file).StartsWith(folder, StringComparison.Ordinal)

/// Fantomas assumes the input files are UTF-8
/// As is stated in F# language spec: https://fsharp.org/specs/language-spec/4.1/FSharpSpec-4.1-latest.pdf#page=25
let hasByteOrderMark (file: string) : Async<bool> =
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

let invalidResultException (file: string) : FormatException =
    FormatException($"Formatting %s{file} leads to invalid F# code")

/// Format content that has already been read, and write the result back over `fileName`. The
/// byte order mark the file started with is put back, since formatting is not the place to
/// decide a file's encoding. With `force`, output that failed the validity check is written
/// anyway and reported as a success.
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
        | FormatResult.IgnoredFile file as r ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = invalidResultException file
            return FormatResult.Error(file, ex)
    }

/// Format a file and write the result to `tw`. A file that needed no change is copied across
/// verbatim rather than skipped, so the caller receives the full content whatever the outcome.
let processSourceFile (force: bool) (profile: bool) inFile (tw: TextWriter) =
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
            let! input = inFile |> File.ReadAllTextAsync |> Async.AwaitTask
            do! input |> tw.WriteAsync |> Async.AwaitTask
            return r
        | FormatResult.IgnoredFile file as r ->
            logGrEqDetailed $"'%s{file}' was ignored"
            return r
        | FormatResult.Error _ as r -> return r
        | FormatResult.InvalidCode(file, _) ->
            let ex = invalidResultException file
            return FormatResult.Error(file, ex)
    }

// The context lines a parse failure's snippet is drawn from come from the file itself. This is
// the error path's last act before the tool gives up on the file, so reading it again is free.
let sourceOf (file: string) : string =
    try
        File.ReadAllText file
    with _ ->
        String.Empty

/// Write one line per file that needs formatting, and a described failure for every file that
/// could not be formatted at all.
let reportCheckResults (checkResult: CheckResult) =
    for filename, exn in checkResult.Errors do
        match Diagnostics.describeParseFailure filename (sourceOf filename) exn with
        | Some parseFailure -> elog parseFailure
        | None -> elog $"error: Failed to format %s{filename}: %s{exn.ToString()}"

    for filename in checkResult.Formatted do
        stdlog $"%s{filename} needs formatting"

/// Report which files need formatting and write nothing. Returns the exit code the process
/// should end with: 0 when every file is already formatted, 99 when at least one needs
/// formatting, and 1 when an input path was unusable or a file could not be formatted.
let runCheckCommand (inputPath: InputPath) : int =
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
    | InputPath.NoFSharpFile s ->
        elog $"Input path '%s{s}' is unsupported file type"
        1
    | InputPath.NotFound s ->
        elog $"Input path '%s{s}' not found"
        1
    | InputPath.Unspecified ->
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

/// Decide what the paths on the command line name. A single path is classified by asking the
/// file system about it. Several paths are all required to exist first, and are then told apart
/// by whether they carry a file extension, so `src` is taken as a folder and `src.fs` as a file.
let classifyInputPath (maybeInput: string list option) : InputPath =
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
        let missing =
            inputs |> List.tryFind (fun x -> not (Directory.Exists(x) || File.Exists(x)))

        match missing with
        | Some x -> InputPath.NotFound x
        | None ->
            let isFolder (path: string) =
                String.IsNullOrWhiteSpace(Path.GetExtension(path))

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

            InputPath.Multiple(loop inputs id)
    | None -> InputPath.Unspecified

/// Format `inFile` and write the result to `outFile`, creating the folders leading up to it if
/// they are not there yet.
// The formatted text is collected in memory and the output file is opened only once there
// is something to put in it. Opening it up front truncates it before the input is read,
// which empties the input when both paths turn out to name the same file, and leaves a
// zero byte file behind whenever formatting does not complete.
let fileToFile (force: bool) (profile: bool) (inFile: string) (outFile: string) : Async<FormatResult> =
    async {
        logGrEqDetailed $"Processing %s{inFile}"
        use buffer = new StringWriter()
        let! processResult = processSourceFile force profile inFile buffer

        match processResult with
        | FormatResult.Formatted _
        | FormatResult.Unchanged _ ->
            let! hasByteOrderMark = hasByteOrderMark inFile
            ensureParentFolderExists outFile
            let contents = buffer.ToString()

            if hasByteOrderMark then
                do! File.WriteAllTextAsync(outFile, contents, Encoding.UTF8) |> Async.AwaitTask
            else
                do! File.WriteAllTextAsync(outFile, contents) |> Async.AwaitTask

            logGrEqDetailed $"%s{outFile} has been written."
        | FormatResult.IgnoredFile _
        | FormatResult.InvalidCode _
        | FormatResult.Error _ -> ()

        return processResult
    }

/// Format one file to one destination. Writing back over the input is a different operation
/// from writing somewhere else, and is kept apart here: in place, the content has to be read
/// before anything is written. A failure is returned as an `Error` result rather than raised, so
/// that one unformattable file does not abandon the rest of the run.
let processFile (force: bool) (profile: bool) (inputFile: string) (outputFile: string) : Async<FormatResult> =
    async {
        try
            if not (isSamePath inputFile outputFile) then
                return! fileToFile force profile inputFile outputFile
            else
                logGrEqDetailed $"Processing %s{inputFile}"
                let! content = File.ReadAllTextAsync inputFile |> Async.AwaitTask
                let config = EditorConfig.readConfiguration inputFile
                return! processSourceString force profile content inputFile config
        with e ->
            return FormatResult.Error(inputFile, e)
    }

/// Format every F# file below `inputFolder`, writing each one to the matching place under
/// `outputFolder` so that the output mirrors the input tree. When the two folders name the same
/// place the files are formatted where they lie.
let processFolder
    (force: bool)
    (profile: bool)
    (inputFolder: string)
    (outputFolder: string)
    : Async<FormatResult> list =
    if not <| Directory.Exists(outputFolder) then
        Directory.CreateDirectory(outputFolder) |> ignore

    let inPlace = isSamePath inputFolder outputFolder

    findAllFilesRecursively inputFolder
    // An output folder inside the input folder is walked over as well, so the previous run's
    // results would be formatted again and nested one level deeper every time.
    |> Seq.filter (fun i -> inPlace || not (isInFolder outputFolder i))
    |> Seq.toList
    |> List.map (fun i ->

        let o =
            if inPlace then
                i
            else
                // The output folder mirrors the input tree. Keeping only the file name would
                // let two files with the same name in different subfolders overwrite each other.
                // fileToFile creates the folders leading up to the file.
                Path.Combine(outputFolder, Path.GetRelativePath(inputFolder, i))

        processFile force profile i o)

/// Format an explicit list of files along with every F# file below the given folders. All of it
/// is formatted in place, which is the only thing on offer once more than one input path is
/// named.
let filesAndFolders
    (force: bool)
    (profile: bool)
    (files: string list)
    (folders: string list)
    : Async<FormatResult> list =
    let fileTasks =
        files
        |> List.map (fun file ->
            if (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) file) then
                logGrEqDetailed $"'%s{file}' was ignored"
                async.Return(FormatResult.IgnoredFile(file))
            else
                processFile force profile file file)

    let folderTasks =
        folders
        |> List.collect (fun folder -> processFolder force profile folder folder)

    (fileTasks @ folderTasks)

/// Sort the results into what was formatted, ignored, left unchanged, and failed. Code that came
/// out invalid is counted as a failure and carries an exception saying so, which leaves the
/// caller one kind of thing to report rather than two.
let partitionResults
    (results: #(FormatResult seq))
    : (string * ProfileInfo option) list * string list * (string * ProfileInfo option) list * (string * Exception) list =
    (([], [], [], []), results)
    ||> Seq.fold (fun (oks, ignores, unchanged, errors) next ->
        match next with
        | FormatResult.Formatted(file, _, p) -> ((file, p) :: oks, ignores, unchanged, errors)
        | FormatResult.IgnoredFile i -> (oks, i :: ignores, unchanged, errors)
        | FormatResult.Unchanged(file, p) -> (oks, ignores, (file, p) :: unchanged, errors)
        | FormatResult.Error(file, e) -> (oks, ignores, unchanged, (file, e) :: errors)
        | FormatResult.InvalidCode(file, _) ->
            let ex = invalidResultException file
            (oks, ignores, unchanged, (file, ex :> Exception) :: errors))

/// Describe one failure on standard error. A parse failure is reported in full, positions and
/// surrounding source included. Anything else is reduced to a single line, unless the verbosity
/// asks for the whole exception.
let reportError (verbosity: VerbosityLevel) (file: string, exn: Exception) : unit =
    let describeOther () : string =
        let message =
            match verbosity with
            | VerbosityLevel.Normal ->
                match exn with
                | :? DefineParseException as dpe ->
                    let combinations =
                        dpe.Combinations
                        |> List.map (fun c -> if c = "no defines" then "no defines" else $"[%s{c}]")
                        |> String.concat ", "

                    $"When Fantomas encounters #if directives in a file, it tries to format all possible combinations of defines and will merge all different versions back into one.\nFor %s{combinations}, however, we were not able to parse the file.\nWhile you may not use this combination in your project, Fantomas requires it to produce valid code.\nConsider fixing the code or ignoring this file.\nFor more information see: https://fsprojects.github.io/fantomas/docs/end-users/ConditionalCompilationDirectives.html"
                | :? FormatException as fe -> fe.Message
                | _ -> ""
            | VerbosityLevel.Detailed -> $"%A{exn}"

        if String.IsNullOrEmpty message then
            $"Failed to format file: %s{file}"
        else
            $"Failed to format file: %s{file} : %s{message}"

    // A parse failure describes itself, positions and all, rather than being reduced to a
    // single line saying only that it happened.
    match Diagnostics.describeParseFailure file (sourceOf file) exn with
    | Some parseFailure -> elog parseFailure
    | None -> elog (describeOther ())

let reportProfileInfo (profile: bool) (file: string, profileInfo: ProfileInfo option) : unit =
    match profile, profileInfo with
    | true, Some pI -> stdlog $"%s{file} Line count: %d{pI.LineCount} Time taken %A{pI.TimeTaken}"
    | _ -> ()

/// Print the line count and the time taken per file, when profiling was asked for.
let reportProfileInfos (profile: bool) (results: (string * ProfileInfo option) list) : unit =
    if profile && not (List.isEmpty results) then
        let table = Table().AddColumns([| "File"; "Line count"; "Time taken" |])

        results
        |> List.choose (fun (f, p) -> p |> Option.map (fun p -> f, p))
        |> List.sortBy fst
        |> List.fold
            (fun (t: Table) (f, p) -> t.AddRow([| f; string<int> p.LineCount; p.TimeTaken.ToString("mm\:ss\.fff") |]))
            table
        |> AnsiConsole.Write

/// Report the outcome of a run. A single file is reported as a sentence naming it, and several
/// files as a table of counts, because a table of one row tells the reader less than the sentence
/// does. Returns the exit code the process should end with: 1 when anything failed, 0 otherwise.
let reportFormatResults (profile: bool) (verbosity: VerbosityLevel) (results: #(FormatResult seq)) : int =
    match Seq.tryExactlyOne results with
    | Some singleResult ->
        match singleResult with
        | FormatResult.Formatted(f, _, p) ->
            stdlog $"%s{f} was formatted."
            reportProfileInfo profile (f, p)
            0
        | FormatResult.IgnoredFile f ->
            stdlog $"%s{f} was ignored."
            0
        | FormatResult.Unchanged(f, p) ->
            stdlog $"%s{f} was unchanged."
            reportProfileInfo profile (f, p)
            0
        | FormatResult.Error(f, e) ->
            reportError verbosity (f, e)
            1
        | FormatResult.InvalidCode(f, _) ->
            let ex = invalidResultException f
            reportError verbosity (f, ex)
            1

    | None ->
        let oks, ignored, unchanged, errored = partitionResults results
        let centeredColumn (v: string) = TableColumn(v).Centered()

        Table()
            .AddColumns(
                [| "[green]Formatted[/]"
                   string<int> oks.Length
                   "Ignored"
                   string<int> ignored.Length
                   "[blue]Unchanged[/]"
                   string<int> unchanged.Length
                   "[red]Errored[/]"
                   string<int> errored.Length |]
                |> Array.map centeredColumn
            )
            .SetBorder(TableBorder.MinimalDoubleHead)
        |> AnsiConsole.Write

        for e in errored do
            reportError verbosity e

        reportProfileInfos profile (oks @ unchanged)

        if errored.Length > 0 then 1 else 0

/// Read the `--verbosity` value. `None` means the value was not one Fantomas knows.
let parseVerbosity (value: string option) : VerbosityLevel option =
    match value |> Option.map (fun v -> v.ToLowerInvariant()) with
    | None
    | Some "n"
    | Some "normal" -> Some VerbosityLevel.Normal
    | Some "d"
    | Some "detailed" -> Some VerbosityLevel.Detailed
    | Some _ -> None

let asyncRunner (computations: Async<FormatResult> list) : FormatResult array =
    computations |> Async.Parallel |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    // Argu never gets to render a usage text of its own: HelpPage.exiter answers --help with
    // the Fantomas help page and reduces an argument error to its first line.
    let parser =
        ArgumentParser.Create<Arguments>(programName = "fantomas", errorHandler = HelpPage.exiter)

    let results = parser.ParseCommandLine argv

    let outputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let inputPath = results.TryGetResult <@ Arguments.Input @> |> classifyInputPath

    let force = results.Contains <@ Arguments.Force @>
    let profile = results.Contains <@ Arguments.Profile @>
    let version = results.TryGetResult <@ Arguments.Version @>

    let verbosityLevel =
        match parseVerbosity (results.TryGetResult <@ Arguments.Verbosity @>) with
        | Some level -> level
        | None ->
            // The logger is not up yet, so this cannot go through elog.
            eprintfn "Invalid verbosity level"
            exit 1

    let isDaemon = results.Contains <@ Arguments.Daemon @>

    // In daemon mode standard out carries the JSON-RPC protocol, so the logger must stay off it.
    let verbosity =
        if isDaemon then
            initDaemonLogger verbosityLevel
        else
            initLogger verbosityLevel

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    let check = results.Contains <@ Arguments.Check @>

    let versionLog =
        let version = CodeFormatter.GetVersion()
        $"Fantomas v%s{version}"

    if Option.isNone version then
        logGrEqDetailed versionLog

    if Option.isSome version then
        stdlog versionLog
        0
    elif isDaemon then
        let daemon =
            new FantomasDaemon(Console.OpenStandardOutput(), Console.OpenStandardInput())

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        0
    elif check then
        runCheckCommand inputPath
    else
        try
            match inputPath, outputPath with
            | InputPath.NoFSharpFile s, _ ->
                elog $"Input path '%s{s}' is unsupported file type."
                1
            | InputPath.NotFound s, _ ->
                elog $"Input path '%s{s}' not found."
                1
            | InputPath.Unspecified, _ ->
                elog "Input path is missing. Call with --help for usage information."
                1
            | InputPath.File f, _ when (IgnoreFile.isIgnoredFile (IgnoreFile.current.Force()) f) ->
                logGrEqDetailed $"'%s{f}' was ignored"
                0
            | InputPath.Folder p1, OutputPath.NotKnown ->
                processFolder force profile p1 p1
                |> asyncRunner
                |> reportFormatResults profile verbosity
            | InputPath.File p1, OutputPath.NotKnown ->
                processFile force profile p1 p1
                |> List.singleton
                |> asyncRunner
                |> reportFormatResults profile verbosity
            | InputPath.File p1, OutputPath.IO p2 ->
                processFile force profile p1 p2
                |> List.singleton
                |> asyncRunner
                |> reportFormatResults profile verbosity
            | InputPath.Folder p1, OutputPath.IO p2 ->
                processFolder force profile p1 p2
                |> asyncRunner
                |> reportFormatResults profile verbosity
            | InputPath.Multiple(files, folders), OutputPath.NotKnown ->
                filesAndFolders force profile files folders
                |> asyncRunner
                |> reportFormatResults profile verbosity
            | InputPath.Multiple _, OutputPath.IO _ ->
                elog "Multiple input files are not supported with the --out flag."
                1
        with exn ->
            elog $"%s{exn.Message}"
            1
