module Fantomas.Cmd.Program

open System
open System.IO
open Microsoft.FSharp.Text.Args

open Fantomas
open Fantomas.FormatConfig

/// These are functionalities that should be implemented now or later.
///
/// Options:
///  --force                         Print the source unchanged if it cannot be parsed correctly
///  --help                          Show help
///  --recurse                       If any given file is a directory, recurse it and process all fs/fsx/fsi files
///  --stdin                         Read F# source from standard input
///  --stdout                        Write the formatted source code to standard output

/// Preferences:
///  --indent=[1-10]                 Set number of spaces to use for indentation
///  [+|-]semicolonEOL               Enable/disable semicolons at the end of line (default = true)
///  [+|-]spaceBeforeArgument        Enable/disable spaces before the first argument (default = false)
///  [+|-]spaceBeforeColon           Enable/disable spaces before colons (default = true)
///  [+|-]spaceAfterComma            Enable/disable spaces after commas (default = true)
///  [+|-]spaceAfterSemiColon        Enable/disable spaces after semicolons (default = true)
///  [+|-]indentOnTryWith            Enable/disable indentation on try/with block (default = false)


let forceText = "Print the source unchanged if it cannot be parsed correctly."
let recurseText = "Process the input folder recursively."
let outputText = "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only."

let fsiText = "Read F# source from stdio as F# signatures."
let stdInText = "Read F# source from standard input."
let stdOutText = " Write the formatted source code to standard output."

let indentText = "Set number of spaces for indentation (default = 4). The value is between 1 and 10."

let semicolonEOLText = "Disable semicolons at the end of line (default = true)."
let argumentText = "Enable spaces before the first argument (default = false)."
let colonText = "Disable spaces before colons (default = true)."
let commaText = "Disable spaces after commas (default = true)."
let semicolonText = "Disable spaces after semicolons (default = true)."
let indentOnTryWithText = "Enable indentation on try/with block (default = false)."

let time f =
  let sw = Diagnostics.Stopwatch.StartNew()
  let res = f()
  sw.Stop()
  printfn "Time taken: %O s" sw.Elapsed
  res

type InputPath = 
    | File of string 
    | Folder of string 
    | StdIn of string
    | Unspecified

type OutputPath =
    | IO of string
    | StdOut
    | Notknown

let extensions = set [".fs"; ".fsx"; ".fsi"; ".ml"; ".mli"]

let isFSharpFile s = Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    seq {
        for f in Directory.GetFiles(path) do
            if isFSharpFile f then yield f
        if isRec then
            for d in Directory.GetDirectories(path) do
                yield! allFiles isRec d    
    }

[<EntryPoint>]
let main args =
    let recurse = ref false
    let force = ref false

    let outputPath = ref Notknown
    let inputPath = ref Unspecified

    let fsi = ref false
    let stdIn = ref false
    let stdOut = ref false
    
    let indent = ref 4
    
    let semicolonEOL = ref true
    let spaceBeforeArgument = ref false
    let spaceBeforeColon = ref true
    let spaceAfterComma = ref true
    let spaceAfterSemiColon = ref true
    let indentOnTryWith = ref false

    let handleOutput s =
        outputPath := IO s

    let handleStdOut() =
        outputPath := StdOut

    let handleInput s = 
        if !stdIn then
            inputPath := StdIn s
        elif Directory.Exists(s) then
           inputPath := Folder s
        elif File.Exists(s) && isFSharpFile(s) then
           inputPath := File s
        else
            stderr.WriteLine("Input path should be a file or a folder.")
            exit 1

    let handleIndent i = 
        if i >= 1 && i <= 10 then
            indent := i
        else
            stderr.WriteLine("Number of spaces should be between 1 and 10.")
            exit 1

    let fileToFile inFile (outFile : string) config =
        try
            use buffer = new StreamWriter(outFile)
            time (fun () -> CodeFormatter.processSourceFile inFile buffer config)
            buffer.Flush()
            stdout.WriteLine("{0} has been written.", outFile)
        with
        | exn ->
            stderr.WriteLine("The following exception occurs while formatting {0}: {1}", inFile, exn.ToString())
            if !force then
                File.WriteAllText(outFile, File.ReadAllText(inFile))
                stdout.WriteLine("Force writing original contents to {0}.", outFile)

    let fileToStdOut inFile config =
        try
            use buffer = new StringWriter()
            /// Don't record running time to avoid writing to console
            CodeFormatter.processSourceFile inFile buffer config
            stdout.Write(buffer.ToString())
        with
        | exn ->
            stderr.WriteLine("The following exception occurs while formatting {0}: {1}", inFile, exn.ToString())
            if !force then
                stdout.Write(File.ReadAllText(inFile))

    let stringToFile s (outFile : string) config =
        try
            use buffer = new StreamWriter(outFile)
            time (fun () -> CodeFormatter.processSourceString !fsi s buffer config)
            buffer.Flush()
            stdout.WriteLine("{0} has been written.", outFile)
        with
        | exn ->
            stderr.WriteLine("The following exception occurs while formatting stdin: {0}", exn.ToString())
            if !force then
                File.WriteAllText(outFile, s)
                stdout.WriteLine("Force writing original contents to {0}.", outFile)

    let stringToStdOut s config =
        try
            use buffer = new StringWriter()
            CodeFormatter.processSourceString !fsi s buffer config
            stdout.Write(buffer.ToString())
        with
        | exn ->
            stderr.WriteLine("The following exception occurs while formatting stdin: {0}", exn.ToString())
            if !force then
                stdout.Write(s)

    let options =
        [| ArgInfo("--recurse", ArgType.Set recurse, recurseText);
           ArgInfo("--force", ArgType.Set force, forceText);

           ArgInfo("--fsi", ArgType.Set fsi, fsiText);
           ArgInfo("--stdin", ArgType.Set stdIn, stdInText);
           ArgInfo("--stdout", ArgType.Unit handleStdOut, stdOutText);
           
           /// --out is no need if one specifies --stdout
           ArgInfo("--out", ArgType.String handleOutput, outputText);

           ArgInfo("--indent", ArgType.Int handleIndent, indentText);
           
           ArgInfo("--noSemicolonEOL", ArgType.Clear semicolonEOL, semicolonEOLText);
           ArgInfo("--spaceBeforeArgument", ArgType.Set spaceBeforeArgument, argumentText);           
           ArgInfo("--noSpaceBeforeColon", ArgType.Clear spaceBeforeColon, colonText);
           ArgInfo("--noSpaceAfterComma", ArgType.Clear spaceAfterComma, commaText);
           ArgInfo("--noSpaceAfterSemiColon", ArgType.Clear spaceAfterSemiColon, semicolonText);
           ArgInfo("--indentOnTryWith", ArgType.Set indentOnTryWith, indentOnTryWithText); |]

    ArgParser.Parse(options, handleInput, "Fantomas <input_path>")

    let config = { FormatConfig.Default with 
                    IndentSpaceNum = !indent;
                    SemicolonAtEndOfLine = !semicolonEOL; 
                    SpaceBeforeArgument = !spaceBeforeArgument; 
                    SpaceBeforeColon = !spaceBeforeColon;
                    SpaceAfterComma = !spaceAfterComma; 
                    SpaceAfterSemicolon = !spaceAfterSemiColon; 
                    IndentOnTryWith = !indentOnTryWith }

    /// Handle inputs via pipeline
    let isKeyAvailable = ref false

    try
        isKeyAvailable := Console.KeyAvailable
    with
    | :? InvalidOperationException ->
        /// Currently only support UTF8
        Console.InputEncoding <- Text.Encoding.UTF8
        inputPath := StdIn(stdin.ReadToEnd())

    match !inputPath, !outputPath with
    | Unspecified, _ ->
        stderr.WriteLine("Input path is missing.")
        exit 1
    | _, Notknown ->
        stderr.WriteLine("Output path is missing.")
        exit 1
    | File p1, IO p2 ->
        fileToFile p1 p2 config
    | Folder p1, IO p2 ->
        if not <| Directory.Exists(p2) then
            Directory.CreateDirectory(p2) |> ignore
        allFiles !recurse p1
        |> Seq.iter (fun i ->     
            /// s supposes to have form s1/suffix
            let suffix = i.Substring(p1.Length + 1)
            let o = Path.Combine(p2, suffix)
            fileToFile i o config)
    | StdIn s, IO p ->
        stringToFile s p config
    | StdIn s, StdOut ->
        stringToStdOut s config
    | File p, StdOut -> 
        fileToStdOut p config
    | Folder p, StdOut ->
        allFiles !recurse p
        |> Seq.iter (fun p -> fileToStdOut p config)
    0
