module Fantomas.Arguments

open System
open System.IO.Abstractions
open Fantomas.Logging
open Fantomas.Paths

type Arguments =
    | Force
    | Out of string
    | Check
    | Json
    | Daemon
    | Version
    | Help
    | Verbosity of string
    | Input of string list

[<RequireQualifiedAccess; Struct>]
type Command =
    | Format
    | Check
    | Profile
    | Daemon

// The one place a command is spelled. A folder called `profile` is shadowed by this, which is the
// price every tool with subcommands pays and which `git`, `docker` and `dotnet` all pay too.
let commands: (string * Command) list =
    [ "check", Command.Check; "profile", Command.Profile; "daemon", Command.Daemon ]

let isCommandName (name: string) : bool =
    commands |> List.exists (fun (command: string, _) -> command = name)

let splitCommand (argv: string array) : Command * string array =
    // One lookup falling through to another, which is what the repository's
    // `fsharp_experimental_keep_indent_in_branch` is for: the second match reads at the same
    // indentation as the first rather than a level in.
    match Array.tryHead argv with
    | None -> Command.Format, argv
    | Some first ->

    match List.tryFind (fun (name: string, _) -> name = first) commands with
    | None -> Command.Format, argv
    | Some(_, command) -> command, Array.skip 1 argv

[<RequireQualifiedAccess>]
type ArgumentProblem =
    | UnknownFlag of flag: string * suggestion: string option
    | MissingValue of flag: string * found: string option
    | UnexpectedValue of flag: string * value: string
    | UnreadableValue of flag: string * value: string * accepted: string list
    | RefusedWithCommand of command: Command * refused: string list

// What a flag does with the token after it. A switch is the flag on its own; a valued flag needs
// one and builds the argument from it.
[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FlagKind =
    | Switch of argument: Arguments
    | Valued of build: (string -> Arguments)

[<NoComparison; NoEquality>]
type Flag =
    {
        Spellings: string list
        Kind: FlagKind
    }

// The one place a flag is spelled. The help page lists the same flags for the reader, and
// `describeArgument` names them back in a message; this is what decides whether a token is one.
let flags: Flag list =
    [
        {
            Spellings = [ "--check" ]
            Kind = FlagKind.Switch Arguments.Check
        }
        {
            Spellings = [ "--out" ]
            Kind = FlagKind.Valued Arguments.Out
        }
        {
            Spellings = [ "--force" ]
            Kind = FlagKind.Switch Arguments.Force
        }
        {
            Spellings = [ "--json" ]
            Kind = FlagKind.Switch Arguments.Json
        }
        {
            Spellings = [ "--daemon" ]
            Kind = FlagKind.Switch Arguments.Daemon
        }
        {
            Spellings = [ "--verbosity"; "-v" ]
            Kind = FlagKind.Valued Arguments.Verbosity
        }
        {
            Spellings = [ "--version" ]
            Kind = FlagKind.Switch Arguments.Version
        }
        {
            Spellings = [ "--help"; "-h" ]
            Kind = FlagKind.Switch Arguments.Help
        }
    ]

let spellings: string list =
    flags |> List.collect (fun (flag: Flag) -> flag.Spellings)

/// Close enough that naming the other one is help rather than noise. `--chek` is one edit from
/// `--check`; beyond two the guess is worse than silence, and `--nope` reaches nothing.
[<Literal>]
let MaximumFlagTypoDistance: int = 2

/// The nearest long flag to a misspelling, when there is one close enough.
///
/// Only long flags, on both sides. A short flag is two characters, so one edit is half of it and
/// every short flag is that close to every other: `-x` came back as "did you mean `-v`", which is
/// a guess dressed as help.
let flagSuggestion (name: string) : string option =
    let isLong (flag: string) : bool =
        flag.StartsWith("--", StringComparison.Ordinal)

    if not (isLong name) then
        None
    else

    // A flag that is really a command, which is what `--profile` became. Worth answering outright
    // rather than through an edit distance: it is the spelling everyone already has in their
    // scripts, and the nearest actual flag to it is nothing like it.
    let trimmed: string = name.TrimStart('-')

    if isCommandName trimmed then
        Some trimmed
    else
        Suggestion.nearest MaximumFlagTypoDistance (List.filter isLong spellings) name

// A lone dash is a path, not a flag, and `--` is handled before this is asked.
let isFlagToken (token: string) : bool =
    token.Length > 1 && token.StartsWith("-", StringComparison.Ordinal)

let tryFindFlag (name: string) : Flag option =
    flags |> List.tryFind (fun (flag: Flag) -> List.contains name flag.Spellings)

// `--out=build` as well as `--out build`. Argu accepted only the second, and every other tool in
// reach accepts both.
let splitAttachedValue (token: string) : string * string option =
    match token.IndexOf('=') with
    | -1 -> token, None
    | at -> token.Substring(0, at), Some(token.Substring(at + 1))

let describeArgument (argument: Arguments) : string =
    match argument with
    | Arguments.Force -> "--force"
    | Arguments.Out _ -> "--out"
    | Arguments.Check -> "--check"
    | Arguments.Json -> "--json"
    | Arguments.Daemon -> "--daemon"
    | Arguments.Version -> "--version"
    | Arguments.Help -> "--help"
    | Arguments.Verbosity _ -> "--verbosity"
    | Arguments.Input _ -> "input paths"

let parse (argv: string array) : Result<Arguments list, ArgumentProblem> =
    let given: ResizeArray<Arguments> = ResizeArray()
    let inputs: ResizeArray<string> = ResizeArray()
    let mutable problem: ArgumentProblem option = None
    let mutable index: int = 0
    let mutable endOfFlags: bool = false

    while problem.IsNone && index < argv.Length do
        let token: string = argv.[index]
        index <- index + 1

        if endOfFlags || not (isFlagToken token) then
            inputs.Add token
        elif token = "--" then
            // The conventional end of the flags. Everything after it is a path, whatever it looks
            // like, which is the only way to name a file that begins with a dash.
            endOfFlags <- true
        else

        let name, attached = splitAttachedValue token

        match tryFindFlag name with
        | None -> problem <- Some(ArgumentProblem.UnknownFlag(name, flagSuggestion name))
        | Some flag ->
            match flag.Kind, attached with
            | FlagKind.Switch argument, None -> given.Add argument
            | FlagKind.Switch _, Some value -> problem <- Some(ArgumentProblem.UnexpectedValue(name, value))
            | FlagKind.Valued build, Some value -> given.Add(build value)
            | FlagKind.Valued build, None ->
                // A token beginning with a dash is never swallowed as a value: `--out --check` is
                // a missing output path, not an output path named `--check`.
                if index < argv.Length && not (isFlagToken argv.[index]) then
                    given.Add(build argv.[index])
                    index <- index + 1
                else
                    let found: string option = if index < argv.Length then Some argv.[index] else None

                    problem <- Some(ArgumentProblem.MissingValue(name, found))

    match problem with
    | Some problem -> Error problem
    | None ->

    // Repeating a flag is allowed and the last one wins, which is the Unix norm and stops a script
    // that accumulates its arguments from failing on a duplicate. Reversing, keeping the first of
    // each, and reversing back is what leaves the last one where the last one was.
    let settled: Arguments list =
        given |> List.ofSeq |> List.rev |> List.distinctBy describeArgument |> List.rev

    if inputs.Count = 0 then
        Ok settled
    else
        Ok(settled @ [ Arguments.Input(List.ofSeq inputs) ])

// `a, b or c`, so a message can name what a flag will take without reading like a data structure.
let listOfWords (words: string list) : string =
    match List.rev words with
    | [] -> ""
    | [ only ] -> only
    | last :: rest -> String.Concat(String.concat ", " (List.rev rest), " or ", last)

let describeArgumentProblem (problem: ArgumentProblem) : string =
    match problem with
    | ArgumentProblem.UnknownFlag(flag, None) -> $"'%s{flag}' is not a Fantomas flag."
    | ArgumentProblem.UnknownFlag(flag, Some suggestion) ->
        if isCommandName suggestion then
            let invocation: string = Invocation.name ()

            $"'%s{flag}' is not a Fantomas flag. '%s{suggestion}' is a command: try '%s{invocation} %s{suggestion} <paths>'."
        else
            $"'%s{flag}' is not a Fantomas flag. Did you mean '%s{suggestion}'?"
    | ArgumentProblem.UnexpectedValue(flag, value) -> $"'%s{flag}' takes no value, but was given '%s{value}'."
    | ArgumentProblem.MissingValue(flag, None) -> $"'%s{flag}' must be followed by a value."
    | ArgumentProblem.MissingValue(flag, Some found) ->
        $"'%s{flag}' must be followed by a value, but was followed by '%s{found}'."
    | ArgumentProblem.UnreadableValue(flag, value, accepted) ->
        $"'%s{flag}' does not accept '%s{value}'. It accepts %s{listOfWords accepted}."
    | ArgumentProblem.RefusedWithCommand(command, refused) ->
        let named: string = String.concat ", " refused

        match command with
        // Worded for the daemon rather than for the spelling, because there are two spellings of
        // it and the reason is the same for both.
        | Command.Daemon ->
            $"A daemon cannot be combined with %s{named}. It is told what to format over JSON-RPC on standard in and answers on standard out, so there is nothing else for it to do and no stream left to report on."
        | Command.Format
        | Command.Check
        | Command.Profile ->
            let spelling: string =
                commands
                |> List.tryPick (fun (name: string, named: Command) -> if named = command then Some name else None)
                |> Option.defaultValue "this command"

            $"'%s{spelling}' cannot be combined with %s{named}."

[<RequireQualifiedAccess>]
type InputPath =
    | File of string
    | Folder of string
    | Multiple of files: string list * folder: string list
    | NoFSharpFile of string
    | NotFound of string

[<RequireQualifiedAccess; Struct>]
type OutputPath =
    | IO of string
    | NotKnown

let classifyInputPath (fs: IFileSystem) (maybeInput: string list option) : InputPath =
    // A run that names no path works on the folder it was started in. `ruff format` and
    // `dotnet format` both read a bare invocation that way, and the alternative was to refuse it,
    // which taught nobody anything they could not have been told by doing the obvious thing.
    let inputs: string list =
        match maybeInput with
        | Some(_ :: _ as inputs) -> inputs
        | Some []
        | None -> [ "." ]

    match inputs with
    | [ input ] ->
        if fs.Directory.Exists(input) then
            InputPath.Folder input
        elif fs.File.Exists input && isFSharpFile input then
            InputPath.File input
        elif fs.File.Exists input then
            InputPath.NoFSharpFile input
        else
            InputPath.NotFound input
    | inputs ->
        let missing: string option =
            inputs
            |> List.tryFind (fun x -> not (fs.Directory.Exists(x) || fs.File.Exists(x)))

        match missing with
        | Some x -> InputPath.NotFound x
        | None ->
            // Every path here is known to exist, so which of the two it is can be asked rather
            // than guessed from whether it carries an extension. Guessing called a folder named
            // `my.stuff` a file, and a file with no extension a folder.
            let folders, files =
                inputs |> List.partition (fun (path: string) -> fs.Directory.Exists path)

            InputPath.Multiple(files, folders)

let tryOut (given: Arguments list) : string option =
    given
    |> List.tryPick (
        function
        | Arguments.Out out -> Some out
        | _ -> None
    )

let tryVerbosity (given: Arguments list) : string option =
    given
    |> List.tryPick (
        function
        | Arguments.Verbosity level -> Some level
        | _ -> None
    )

let tryInput (given: Arguments list) : string list option =
    given
    |> List.tryPick (
        function
        | Arguments.Input input -> Some input
        | _ -> None
    )

let describeInputPaths (inputPath: InputPath) : string =
    match inputPath with
    | InputPath.File file -> file
    | InputPath.Folder folder -> folder
    | InputPath.NoFSharpFile path -> path
    | InputPath.NotFound path -> path
    | InputPath.Multiple(files, folders) -> String.concat " " (List.append files folders)

let answersAndExits (argument: Arguments) : bool =
    match argument with
    | Arguments.Version
    | Arguments.Help -> true
    | _ -> false

let argumentFor (spelling: string) : Arguments option =
    tryFindFlag spelling
    |> Option.map (fun (flag: Flag) ->
        match flag.Kind with
        | FlagKind.Switch argument -> argument
        // The value is a placeholder. This answers what a flag is, not what it was given.
        | FlagKind.Valued build -> build ""
    )

let commandSpelledBy (argument: Arguments) : Command option =
    match argument with
    | Arguments.Check -> Some Command.Check
    | Arguments.Daemon -> Some Command.Daemon
    | _ -> None

let argumentsRefusedBy (command: Command) (given: Arguments list) : string list =
    // A flag that answers and exits wins rather than being refused, and `--verbosity` sets the
    // level the run logs at, which is something every command does.
    let keptByEveryCommand (argument: Arguments) : bool =
        match argument with
        | Arguments.Verbosity _ -> true
        | _ -> answersAndExits argument

    let keptByThisCommand (argument: Arguments) : bool =
        match command with
        | Command.Format -> true
        // A check reads the same files a format run does and reports the same two ways, but writes
        // nothing, so where to put output and whether to write invalid code have nothing to apply
        // to. `--check` is kept because it is one of the two ways of asking.
        | Command.Check ->
            match argument with
            | Arguments.Check
            | Arguments.Json
            | Arguments.Input _ -> true
            | _ -> false
        // A daemon is told what to format over JSON-RPC on standard in and answers on standard
        // out, so nothing that says what to format, where to put it, or how to report it has
        // anything to apply to. `--daemon` is kept because it is one of the two ways of asking.
        | Command.Daemon -> argument = Arguments.Daemon
        // A profile takes paths and reports the two ways every command reports. What it has no use
        // for is where to put output and whether to write invalid code, because it writes nothing.
        | Command.Profile ->
            match argument with
            | Arguments.Json
            | Arguments.Input _ -> true
            | _ -> false

    // Distinct because `--out a --out b` is two results and one complaint, and sorted so that the
    // order the flags were typed in does not change the message.
    given
    |> List.choose (fun (argument: Arguments) ->
        if keptByEveryCommand argument || keptByThisCommand argument then
            None
        else
            Some(describeArgument argument)
    )
    |> List.distinct
    |> List.sort

let parseVerbosity (value: string) : VerbosityLevel option =
    match value.ToLowerInvariant() with
    | "n"
    | "normal" -> Some VerbosityLevel.Normal
    | "d"
    | "detailed" -> Some VerbosityLevel.Detailed
    | _ -> None
