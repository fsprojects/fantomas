module Fantomas.Tests.TestHelpers

open System
open System.Diagnostics
open System.IO
open System.IO.Abstractions
open System.Text
open Serilog
open Serilog.Core
open Serilog.Events
open Spectre.Console
open Fantomas.Theme
open Fantomas
open Fantomas.Cli
open Fantomas.Core
open Fantomas.Logging
open Fantomas.Daemon

[<RequireQualifiedAccess>]
module String =
    let normalizeNewLine (str: string) =
        str.Replace("\r\n", "\n").Replace("\r", "\n")

type TemporaryFileCodeSample
    internal
    (
        codeSnippet: string,
        ?hasByteOrderMark: bool,
        ?fileName: string,
        ?subFolder: string,
        ?subFolders: string array,
        ?extension: string
    )
    =
    let hasByteOrderMark = defaultArg hasByteOrderMark false

    let internalSubFolders =
        match subFolders with
        | Some sf -> Some sf
        | None ->
            match subFolder with
            | Some sf -> Array.singleton sf |> Some
            | None -> None

    let filename =
        let name =
            match fileName with
            | Some fn -> fn
            | None -> Guid.NewGuid().ToString()

        let extension = Option.defaultValue "fs" extension

        match internalSubFolders with
        | None -> Path.Join(Path.GetTempPath(), sprintf "%s.%s" name extension)
        | Some sf ->
            let tempFolder = Path.Join(Path.GetTempPath(), Path.Join(sf))

            if not (Directory.Exists(tempFolder)) then
                Directory.CreateDirectory(tempFolder) |> ignore

            Path.Join(tempFolder, sprintf "%s.%s" name extension)

    do
        (if hasByteOrderMark then
             File.WriteAllText(filename, codeSnippet, Encoding.UTF8)
         else
             File.WriteAllText(filename, codeSnippet))

    member _.Filename: string = filename

    interface IDisposable with
        member this.Dispose() : unit =
            File.Delete(filename)

            internalSubFolders
            |> Option.iter (fun sf ->
                let path = Path.Join(Path.GetTempPath(), sf.[0])
                Directory.Delete(path, true)
            )

type OutputFile internal () =
    let filename = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString() + ".fs")

    member _.Filename: string = filename

    interface IDisposable with
        member this.Dispose() : unit =
            if File.Exists(filename) then
                File.Delete(filename)

type OutputFolder internal () =
    let foldername = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString())

    member _.Foldername: string = foldername

    interface IDisposable with
        member this.Dispose() : unit =
            if Directory.Exists(foldername) then
                Directory.Delete(foldername, true)

/// A folder of its own, holding an `.editorconfig` and one F# file it applies to.
///
/// A test that reads configuration from disk needs the two next to each other. Writing the
/// `.editorconfig` straight into the temp folder, as this used to, puts it where every other test
/// doing the same finds it. `root = true` goes in front of the content so the chain stops here
/// rather than picking up whatever sits above the temp folder on this machine.
type ConfiguredCodeSample internal (editorConfig: string, codeSnippet: string, ?extension: string) =
    let folder = Path.Join(Path.GetTempPath(), Guid.NewGuid().ToString("N"))
    do Directory.CreateDirectory folder |> ignore

    let editorConfigFile = Path.Join(folder, ".editorconfig")

    let filename =
        Path.Join(folder, sprintf "File.%s" (Option.defaultValue "fs" extension))

    do
        File.WriteAllText(editorConfigFile, "root = true\n" + editorConfig)
        File.WriteAllText(filename, codeSnippet)

    member _.Filename: string = filename
    member _.EditorConfigFile: string = editorConfigFile

    interface IDisposable with
        member this.Dispose() : unit = Directory.Delete(folder, true)

type FantomasIgnoreFile internal (content: string) =
    let filename = Path.Join(Path.GetTempPath(), IgnoreFile.IgnoreFileName)

    do File.WriteAllText(filename, content)
    member _.Filename: string = filename

    interface IDisposable with
        member this.Dispose() : unit =
            if File.Exists(filename) then
                File.Delete(filename)

/// Create every file, and the folders leading to it, in the given file system.
let makeFileHierarchy (fs: IFileSystem) (filePaths: string list) : unit =
    for path in filePaths do
        let fileInfo: IFileInfo = fs.FileInfo.New path
        fileInfo.Directory.Create()
        fs.File.WriteAllText(fileInfo.FullName, "some text")

/// The root a `MockFileSystem` hangs its paths from, so that a test never writes a path by hand
/// and never has to know which platform it is running on.
let mockRoot (fs: IFileSystem) : string =
    fs.Path.GetTempPath() |> fs.Path.GetPathRoot

/// A logger that writes nowhere, for a test that does not care what was logged.
let silentLogger: ILogger = LoggerConfiguration().CreateLogger()

/// What a run wrote, gathered per level rather than per stream: which stream a level lands on is
/// `Logging.createLogger`'s business, and this is about the messages.
type CollectedLog =
    {
        Information: string list
        Warning: string list
        Error: string list
        Fatal: string list
        Debug: string list
    }

type private CollectingSink() =
    let events: ResizeArray<LogEvent> = ResizeArray()

    member _.Events: LogEvent list = lock events (fun () -> List.ofSeq events)

    interface ILogEventSink with
        member _.Emit(logEvent: LogEvent) : unit =
            lock events (fun () -> events.Add logEvent)

/// A logger that keeps what was written, and a way to read it back.
let collectingLogger () : ILogger * (unit -> CollectedLog) =
    let sink: CollectingSink = CollectingSink()

    let logger: ILogger =
        LoggerConfiguration().MinimumLevel.Verbose().WriteTo.Sink(sink).CreateLogger()

    let collected () : CollectedLog =
        let atLevel (level: LogEventLevel) : string list =
            sink.Events
            |> List.choose (fun (e: LogEvent) -> if e.Level = level then Some(e.RenderMessage()) else None)

        {
            Information = atLevel LogEventLevel.Information
            Warning = atLevel LogEventLevel.Warning
            Error = atLevel LogEventLevel.Error
            Fatal = atLevel LogEventLevel.Fatal
            Debug = atLevel LogEventLevel.Debug
        }

    logger, collected

/// A Spectre console that draws into a string rather than onto a terminal. Colour and width are
/// pinned so that what it draws does not depend on the terminal the tests happen to run in.
let recordingConsole () : IAnsiConsole * (unit -> string) =
    let writer: StringWriter = new StringWriter()

    let console: IAnsiConsole =
        AnsiConsole.Create(
            AnsiConsoleSettings(
                Ansi = AnsiSupport.No,
                ColorSystem = ColorSystemSupport.NoColors,
                Out = AnsiConsoleOutput(writer)
            )
        )

    console.Profile.Width <- 200
    console, (fun () -> writer.ToString())

/// No colour and the ascii glyphs, so a test asserts on what was said rather than on how it was
/// drawn. What the theme does with each of those is settled in `ThemeTests`.
let plainTheme: Theme =
    {
        Palette = Palette.NoColour
        Glyphs = GlyphSet.Ascii
    }

/// A `CliEnvironment` that keeps whatever a run writes, with the two ways to read it back.
[<NoComparison; NoEquality>]
type RecordedRun =
    {
        Environment: CliEnvironment
        /// What was logged, per level.
        Log: unit -> CollectedLog
        /// What Spectre drew, as text.
        Drawn: unit -> string
    }

/// An environment over the given file system that records rather than prints, honouring the given
/// ignore file and reading no `.editorconfig`.
let recordingEnvironment (fs: IFileSystem) (ignoreFile: IgnoreFile option) : RecordedRun =
    let logger, collected = collectingLogger ()
    let console, drawn = recordingConsole ()

    {
        Environment =
            {
                FileSystem = fs
                IgnoreFile = ignoreFile
                ReadConfiguration =
                    fun _ ->
                        { FormatConfig.Default with
                            EndOfLine = EndOfLineStyle.LF
                        }
                Log = logger
                Console = console
                OutputTheme = plainTheme
                ErrorTheme = plainTheme
            }
        Log = collected
        Drawn = drawn
    }

/// The settings a run gets when nothing was asked for on the command line.
let defaultSettings: CliSettings =
    {
        Force = false
        Profile = false
        Verbosity = VerbosityLevel.Normal
    }

/// A `DaemonEnvironment` over the given file system, reading whatever configuration is handed in
/// rather than an `.editorconfig` on disk. Enough for a test about what the daemon does with a
/// configuration, as opposed to one about where the configuration came from.
let daemonEnvironment
    (fs: IFileSystem)
    (readConfiguration: string -> EditorConfig.EditorConfigResult option)
    : DaemonEnvironment
    =
    {
        FileSystem = fs
        ReadConfiguration = readConfiguration
        Log = silentLogger
    }

/// A `DaemonEnvironment` over the real file system, as the tool itself builds one.
let realDaemonEnvironment: DaemonEnvironment =
    {
        FileSystem = FileSystem()
        ReadConfiguration = EditorConfig.tryReadConfiguration
        Log = Log.Logger
    }

/// A `CliEnvironment` over the real file system, honouring no ignore file. Enough for a test that
/// wants the tool's own behaviour without standing up a mock.
let realEnvironment: CliEnvironment =
    {
        FileSystem = FileSystem()
        IgnoreFile = None
        ReadConfiguration = EditorConfigReport.readConfiguration (EditorConfigReport.createReporter Log.Logger)
        Log = Log.Logger
        Console = AnsiConsole.Console
        OutputTheme = plainTheme
        ErrorTheme = plainTheme
    }

type FantomasToolResult =
    {
        ExitCode: int
        Output: string
        Error: string
    }

let getFantomasToolStartInfo (arguments: string list) : ProcessStartInfo =
    let configuration: string =
#if DEBUG
        "debug"
#else
        "release"
#endif

    // Resolved from where this file sits rather than from where the test assembly is running,
    // because those are not the same place under a coverage run: AltCover executes the tests from
    // an instrumented copy of the output folder.
    let fantomasDll: string =
        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "artifacts", "bin", "Fantomas", configuration, "fantomas.dll")
        |> Path.GetFullPath

    if not (File.Exists fantomasDll) then
        failwithf $"The fantomas dll at \"%s{fantomasDll}\" does not exist!"

    let argumentArray = fantomasDll :: arguments
    let startInfo = ProcessStartInfo("dotnet", argumentArray)
    startInfo.UseShellExecute <- false
    startInfo.WorkingDirectory <- Path.GetTempPath()
    startInfo.RedirectStandardOutput <- true
    startInfo.RedirectStandardError <- true
    startInfo

let runFantomasToolWithEnvironment (environment: (string * string) list) arguments : FantomasToolResult =
    let startInfo = getFantomasToolStartInfo arguments

    for key, value in environment do
        startInfo.Environment[key] <- value

    use p = Process.Start startInfo

    let output = p.StandardOutput.ReadToEnd()
    let error = p.StandardError.ReadToEnd()
    p.WaitForExit()

    {
        ExitCode = p.ExitCode
        Output = output
        Error = error
    }

let runFantomasTool arguments : FantomasToolResult =
    runFantomasToolWithEnvironment [] arguments

let checkCode (files: string list) : FantomasToolResult =
    let arguments = "--check" :: files
    runFantomasTool arguments

let formatCode (files: string list) : FantomasToolResult = runFantomasTool files
