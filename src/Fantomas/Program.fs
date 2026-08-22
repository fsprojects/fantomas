module Program

open System
open System.IO.Abstractions
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Fantomas.Logging
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.FormatCommand
open Fantomas.Report
open Fantomas.CheckCommand
open Argu
open Serilog
open Spectre.Console

[<EntryPoint>]
let main argv =
    // Argu never gets to render a usage text of its own: HelpPage.exiter answers --help with
    // the Fantomas help page and reduces an argument error to its first line.
    let parser: ArgumentParser<Arguments> =
        ArgumentParser.Create<Arguments>(programName = "fantomas", errorHandler = HelpPage.exiter)

    let results: ParseResults<Arguments> = parser.ParseCommandLine argv

    let outputPath: OutputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let fileSystem: IFileSystem = FileSystem()

    let inputPath: InputPath =
        results.TryGetResult <@ Arguments.Input @> |> classifyInputPath fileSystem

    let force: bool = results.Contains <@ Arguments.Force @>
    let profile: bool = results.Contains <@ Arguments.Profile @>
    let version: Arguments option = results.TryGetResult <@ Arguments.Version @>

    let verbosityLevel: VerbosityLevel =
        match parseVerbosity (results.TryGetResult <@ Arguments.Verbosity @>) with
        | Some level -> level
        | None ->
            // The logger is not up yet, so this cannot go through elog.
            eprintfn "Invalid verbosity level"
            exit 1

    let isDaemon: bool = results.Contains <@ Arguments.Daemon @>

    // In daemon mode standard out carries the JSON-RPC protocol, so the logger must stay off it.
    let verbosity: VerbosityLevel =
        if isDaemon then
            initDaemonLogger verbosityLevel
        else
            initLogger verbosityLevel

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    // The logger the calls above configured, now handed down rather than reached for.
    let log: ILogger = Log.Logger

    let check: bool = results.Contains <@ Arguments.Check @>

    let versionLog: string =
        let version: string = CodeFormatter.GetVersion()
        $"Fantomas v%s{version}"

    if Option.isNone version then
        log.Debug versionLog

    if Option.isSome version then
        log.Information versionLog
        0
    elif isDaemon then
        let daemon: FantomasDaemon =
            new FantomasDaemon(
                Console.OpenStandardOutput(),
                Console.OpenStandardInput(),
                { FileSystem = fileSystem
                  ReadConfiguration = EditorConfig.readConfiguration
                  Log = log }
            )

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        0
    else
        // Reading `.fantomasignore` can fail on a pattern the ignore library will not compile, and
        // building the environment is the first thing either command needs, so it happens under
        // the same guard the commands run under rather than before it.
        try
            let environment: CliEnvironment =
                { FileSystem = fileSystem
                  IgnoreFile =
                    IgnoreFile.findInDirectory
                        fileSystem
                        Environment.CurrentDirectory
                        (IgnoreFile.loadIgnoreList fileSystem)
                  ReadConfiguration = EditorConfig.readConfiguration
                  Log = log
                  Console = AnsiConsole.Console }

            let settings: CliSettings =
                { Force = force
                  Profile = profile
                  Verbosity = verbosity }

            if check then
                runCheckCommand environment inputPath |> reportCheckCommand environment
            else
                runFormatCommand environment settings inputPath outputPath
                |> reportFormatCommand environment settings
        with exn ->
            log.Error $"%s{exn.Message}"
            1
