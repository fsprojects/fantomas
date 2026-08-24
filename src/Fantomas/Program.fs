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
open Fantomas.CommandResult
open Argu
open Serilog
open Spectre.Console

/// Parse the command line and run whichever command it names: printing the version, serving the
/// daemon, checking whether files need formatting, or formatting them. Returns the exit code the
/// process should end with.
[<EntryPoint>]
let main argv =
    // Argu never gets to render a usage text of its own: HelpPage.exiter answers --help with
    // the Fantomas help page and reduces an argument error to its first line.
    let parser: ArgumentParser<Arguments> =
        ArgumentParser.Create<Arguments>(programName = "fantomas", errorHandler = HelpPage.exiter)

    let results: ParseResults<Arguments> = parser.ParseCommandLine argv

    let versionBanner: string =
        let version: string = CodeFormatter.GetVersion()
        $"Fantomas v%s{version}"

    // `--version` answers on its own and stops, whatever else was asked for. Nothing is validated
    // first: a version you can only read once the rest of the command line is already correct is no
    // use for finding out what you are running. It is written straight to standard out rather than
    // through the logger, so it reads the same at any verbosity, and standard out is where
    // Fantomas.Client looks for it when it discovers the tool.
    if results.Contains <@ Arguments.Version @> then
        Console.Out.WriteLine versionBanner
        exit 0

    let outputPath: OutputPath =
        match results.TryGetResult <@ Arguments.Out @> with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let fileSystem: IFileSystem = FileSystem()

    let inputPath: InputPath =
        results.TryGetResult <@ Arguments.Input @> |> classifyInputPath fileSystem

    let force: bool = results.Contains <@ Arguments.Force @>
    let profile: bool = results.Contains <@ Arguments.Profile @>

    let verbosityLevel: VerbosityLevel =
        match parseVerbosity (results.TryGetResult <@ Arguments.Verbosity @>) with
        | Some level -> level
        | None ->
            // The logger is not configured yet, so this cannot go through it.
            eprintfn "Invalid verbosity level"
            exit 1

    let isDaemon: bool = results.Contains <@ Arguments.Daemon @>
    let json: bool = results.Contains <@ Arguments.Json @>

    // Everything here used to be accepted alongside --daemon and then silently ignored.
    match
        if isDaemon then
            argumentsRefusedWithDaemon (results.GetAllResults())
        else
            []
    with
    | [] -> ()
    | refused ->
        // The logger is not configured yet, so this cannot go through it.
        eprintfn
            "--daemon cannot be combined with %s. A daemon is told what to format over JSON-RPC on standard in and answers on standard out, so there is nothing else for it to do and no stream left to report on."
            (String.concat ", " refused)

        eprintfn "Run fantomas --help for usage information."
        exit 1

    // `--json` puts one document on standard out, so the logger moves off it entirely, the way it
    // does in daemon mode, where standard out carries the JSON-RPC protocol.
    let verbosity: VerbosityLevel =
        if isDaemon then initDaemonLogger verbosityLevel
        elif json then initJsonLogger verbosityLevel
        else initLogger verbosityLevel

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    // The logger the calls above configured, now handed down rather than reached for.
    let log: ILogger = Log.Logger

    let check: bool = results.Contains <@ Arguments.Check @>

    log.Debug versionBanner

    if isDaemon then
        let daemon: FantomasDaemon =
            new FantomasDaemon(
                Console.OpenStandardOutput(),
                Console.OpenStandardInput(),
                { FileSystem = fileSystem
                  ReadConfiguration = EditorConfig.tryReadConfiguration
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
                  ReadConfiguration = EditorConfigReport.readConfiguration (EditorConfigReport.createReporter log)
                  Log = log
                  Console = AnsiConsole.Console }

            let settings: CliSettings =
                { Force = force
                  Profile = profile
                  Verbosity = verbosity }

            if check then
                let result: CheckCommandResult = runCheckCommand environment inputPath

                if json then
                    JsonReport.reportCheckCommand Environment.CurrentDirectory Console.Out result
                else
                    reportCheckCommand environment result
            else
                let result: FormatCommandResult =
                    runFormatCommand environment settings inputPath outputPath

                if json then
                    JsonReport.reportFormatCommand Environment.CurrentDirectory Console.Out result
                else
                    reportFormatCommand environment settings result
        with exn ->
            // The document is what a caller asked for, so a run that fell over before it reached a
            // file still gets one, carrying what went wrong. It is the whole report, here as
            // everywhere else, so nothing is logged alongside it.
            if json then
                if check then
                    JsonReport.reportCheckCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (CheckCommandResult.Failed exn)
                else
                    JsonReport.reportFormatCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (FormatCommandResult.Failed exn)
            else
                log.Error $"%s{exn.Message}"
                1
