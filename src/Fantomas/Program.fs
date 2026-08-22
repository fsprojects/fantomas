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

    let check: bool = results.Contains <@ Arguments.Check @>

    let versionLog: string =
        let version: string = CodeFormatter.GetVersion()
        $"Fantomas v%s{version}"

    if Option.isNone version then
        logGrEqDetailed versionLog

    if Option.isSome version then
        stdlog versionLog
        0
    elif isDaemon then
        let daemon: FantomasDaemon =
            new FantomasDaemon(Console.OpenStandardOutput(), Console.OpenStandardInput())

        AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> (daemon :> IDisposable).Dispose())

        daemon.WaitForClose.GetAwaiter().GetResult()
        0
    else
        let environment: CliEnvironment =
            { FileSystem = fileSystem
              IgnoreFile =
                IgnoreFile.findInDirectory
                    fileSystem
                    Environment.CurrentDirectory
                    (IgnoreFile.loadIgnoreList fileSystem)
              ReadConfiguration = EditorConfig.readConfiguration }

        let settings: CliSettings =
            { Force = force
              Profile = profile
              Verbosity = verbosity }

        if check then
            runCheckCommand environment inputPath |> reportCheckCommand environment
        else
            runFormatCommand environment settings inputPath outputPath
            |> reportFormatCommand environment settings
