module Program

open System
// Fantomas.Core has a FormatResult of its own. Opening Fantomas last is what makes the
// FormatResult named here the one this project defines.
open Fantomas.Core
open Fantomas
open Fantomas.Daemon
open Fantomas.Logging
open Fantomas.Arguments
open Fantomas.FormatCommand
open Fantomas.Report
open Fantomas.CheckCommand
open Argu

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
        runCheckCommand inputPath |> reportCheckCommand
    else
        runFormatCommand force profile inputPath outputPath
        |> reportFormatCommand profile verbosity
