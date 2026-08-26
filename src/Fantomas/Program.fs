module Program

open System
open System.IO.Abstractions
open Fantomas.Core
open Fantomas
open Fantomas.Logging
open Fantomas.Arguments
open Fantomas.Cli
open Fantomas.FormatCommand
open Fantomas.Report
open Fantomas.CheckCommand
open Fantomas.DaemonCommand
open Fantomas.ProfileCommand
open Fantomas.DoctorCommand
open Fantomas.CommandResult
open Serilog

/// Parse the command line and run whichever command it names: printing the version, serving the
/// daemon, checking whether files need formatting, or formatting them. Returns the exit code the
/// process should end with.
[<EntryPoint>]
let main argv =
    // How this run was started, resolved once at the edge of the process and handed to everything
    // that has to name a command back. The help pointer is the first of them: a local tool install
    // is given a line it can run rather than one it has to translate.
    let invocation: string = Invocation.name ()

    let usagePointer: string = $"Run %s{invocation} --help for usage information."

    // Every way the command line can be wrong ends here. The logger is not configured yet, and
    // deliberately so: what to configure it with is one of the things being read.
    let refuse (problem: ArgumentProblem) : 'a =
        eprintfn "%s" (describeArgumentProblem invocation problem)
        eprintfn "%s" usagePointer
        exit 1

    // The command is the first token when it names one, and what is left is the ordinary flags and
    // paths. Splitting the two apart keeps the parser about how a command line is written rather
    // than about what any one command does with it.
    let command, rest = splitCommand argv

    let given: Arguments list =
        match parse rest with
        | Ok given -> given
        | Error problem -> refuse problem

    let contains (argument: Arguments) : bool = List.contains argument given

    // `--daemon` and `--check` are the older spellings of the commands of those names, and both go
    // on working. `Fantomas.Client` launches every one of its three ways with `--daemon`, so an
    // editor talking to a Fantomas newer than itself still gets a daemon, and `--check .` is in
    // every pipeline there is. Unlike `--profile`, each flag means exactly what its command means,
    // so keeping them changes nothing about what a run does.
    //
    // Resolved before `--help` is answered, so `fantomas --check --help` is the check page. A
    // daemon wins over a check when both were asked for, so the pair is refused by the command
    // that has the least to say about the other rather than quietly becoming one of them.
    let command: Command =
        match command with
        // The first token named one, so a flag naming another is a contradiction for the refusal
        // rules below to catch rather than something to quietly overrule it with.
        | Command.Format ->
            if contains Arguments.Daemon then Command.Daemon
            elif contains Arguments.Check then Command.Check
            else Command.Format
        | named -> named

    // `--help` and `--version` answer on their own and stop, whatever else was asked for. Nothing
    // is validated first: an answer you can only read once the rest of the command line is already
    // correct is no use for finding out what you are running or how to run it.
    if contains Arguments.Help then
        HelpPage.print command
        exit 0

    let versionBanner: string =
        let version: string = CodeFormatter.GetVersion()
        $"Fantomas v%s{version}"

    // Written straight to standard out rather than through the logger, so it reads the same at any
    // verbosity, and standard out is where Fantomas.Client looks for it when it discovers the tool.
    //
    // Short by default, the way the help page has always shown it: the commit hash is for pasting
    // into a `git show`, and nine characters are what git itself prints. The whole hash is still
    // reachable, at the verbosity that asks for everything.
    //
    // This is the one place the rest of the command line is peeked at before `--version` answers,
    // and it stays compatible with the rule above it: nothing is validated. A `--verbosity` value
    // Fantomas cannot read falls back to the short form rather than refusing to say what this is.
    if contains Arguments.Version then
        let detailed: bool =
            tryVerbosity given |> Option.bind parseVerbosity = Some VerbosityLevel.Detailed

        let banner: string =
            if detailed then
                versionBanner
            else
                $"Fantomas v%s{HelpPage.shortVersion ()}"

        Console.Out.WriteLine banner
        exit 0

    let outputPath: OutputPath =
        match tryOut given with
        | Some output -> OutputPath.IO output
        | None -> OutputPath.NotKnown

    let fileSystem: IFileSystem = FileSystem()

    let inputPath: InputPath = classifyInputPath fileSystem (tryInput given)

    let force: bool = contains Arguments.Force

    let verbosityLevel: VerbosityLevel =
        // Not given at all is the default. Given and unreadable is a mistake worth stopping for,
        // and the two are told apart here rather than inside the parse.
        match tryVerbosity given with
        | None -> VerbosityLevel.Normal
        | Some asked ->

        match parseVerbosity asked with
        | Some level -> level
        | None -> refuse (ArgumentProblem.UnreadableValue("--verbosity", asked, [ "normal"; "detailed"; "n"; "d" ]))

    let isDaemon: bool = command = Command.Daemon
    let json: bool = contains Arguments.Json

    // Every one of these used to be accepted and then silently ignored.
    match argumentsRefusedBy command given with
    | [] -> ()
    | refused -> refuse (ArgumentProblem.RefusedWithCommand(command, refused))

    // Said to a person and to nobody else. Standard error not being redirected is what tells a
    // terminal from a pipeline, and a pipeline is the wrong audience twice over: its log would
    // carry the line on every run for as long as both spellings exist, which is forever, and the
    // person who could change the command line is not reading it.
    //
    // The same test keeps this out of the daemon an editor starts, because `Fantomas.Client`
    // redirects standard error in order to read it.
    if not Console.IsErrorRedirected then
        let olderSpelling: (string * string) option =
            match command with
            | Command.Check when contains Arguments.Check -> Some("--check", "check")
            | Command.Daemon when contains Arguments.Daemon -> Some("--daemon", "daemon")
            | _ -> None

        match olderSpelling with
        | None -> ()
        | Some(flag, verb) -> eprintfn "'%s' still works, and '%s %s' is how it is spelled now." flag invocation verb

    // `--json` puts one document on standard out, so the logger moves off it entirely, the way it
    // does in daemon mode, where standard out carries the JSON-RPC protocol.
    let verbosity: VerbosityLevel =
        if isDaemon then initDaemonLogger verbosityLevel
        elif json then initJsonLogger verbosityLevel
        else initLogger verbosityLevel

    AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> closeAndFlushLog ())

    // The logger the calls above configured, now handed down rather than reached for.
    let log: ILogger = Log.Logger

    let check: bool = command = Command.Check

    log.Debug versionBanner

    if isDaemon then
        runDaemonCommand fileSystem log
    else
        // Reading `.fantomasignore` can fail on a pattern the ignore library will not compile, and
        // building the environment is the first thing either command needs, so it happens under
        // the same guard the commands run under rather than before it.
        try
            let environment: CliEnvironment =
                {
                    FileSystem = fileSystem
                    FindIgnoreFile = IgnoreFile.cachedFinder fileSystem (IgnoreFile.loadIgnoreList fileSystem)
                    FindIgnoreFilesAbove = IgnoreFile.findAbove fileSystem (IgnoreFile.loadIgnoreList fileSystem)
                    ReadConfiguration = EditorConfigReport.readConfiguration (EditorConfigReport.createReporter log)
                    ResolveConfiguration = EditorConfig.resolveConfiguration
                    Log = log
                    OutputTheme = Theme.forOutput ()
                    ErrorTheme = Theme.forError ()
                    Invocation = invocation
                }

            let settings: CliSettings = { Force = force; Verbosity = verbosity }

            if command = Command.Profile then
                let result: ProfileCommandResult = runProfileCommand environment inputPath

                if json then
                    JsonReport.reportProfileCommand Environment.CurrentDirectory Console.Out result
                else
                    reportProfileCommand environment settings inputPath result
            elif command = Command.Doctor then
                let result: DoctorCommandResult = runDoctorCommand environment inputPath

                if json then
                    JsonReport.reportDoctorCommand Environment.CurrentDirectory Console.Out result
                else
                    reportDoctorCommand environment settings result
            elif check then
                let result: CheckCommandResult = runCheckCommand environment inputPath

                if json then
                    JsonReport.reportCheckCommand Environment.CurrentDirectory Console.Out result
                else
                    reportCheckCommand environment inputPath result
            else
                let result: FormatCommandResult =
                    runFormatCommand environment settings inputPath outputPath

                if json then
                    JsonReport.reportFormatCommand Environment.CurrentDirectory Console.Out result
                else
                    reportFormatCommand environment settings inputPath outputPath result
        with exn ->
            // The document is what a caller asked for, so a run that fell over before it reached a
            // file still gets one, carrying what went wrong. It is the whole report, here as
            // everywhere else, so nothing is logged alongside it.
            // The document says which command was asked for, so the failing one has to be the one
            // that reports: a `profile` run that fell over used to hand back a document calling
            // itself a format run.
            if json then
                match command with
                | Command.Profile ->
                    JsonReport.reportProfileCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (ProfileCommandResult.Failed exn)
                | Command.Check ->
                    JsonReport.reportCheckCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (CheckCommandResult.Failed exn)
                | Command.Doctor ->
                    JsonReport.reportDoctorCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (DoctorCommandResult.Failed exn)
                | Command.Format
                | Command.Daemon ->
                    JsonReport.reportFormatCommand
                        Environment.CurrentDirectory
                        Console.Out
                        (FormatCommandResult.Failed exn)
            else
                log.Error $"%s{exn.Message}"
                1
