module Fantomas.Cmd.Program

open System
open System.IO
open Microsoft.FSharp.Text.Args

open Fantomas
open Fantomas.FormatConfig

// These are functionalities that should be implemented now or later.
//
// Options:
//  --force                         Print the source unchanged if it cannot be parsed correctly
//  --help                          Show help
//  --recurse                       If any given file is a directory, recurse it and process all fs/fsx/fsi files
//  --stdin                         Read F# source from standard input
//  --stdout                        Write the formatted source code to standard output

// Preferences:
//  --indent=[1-10]                 Set number of spaces to use for indentation
//  --pageWidth=[60-inf]            Set the column where we break to new lines
//  [+|-]semicolonEOL               Enable/disable semicolons at the end of line (default = false)
//  [+|-]spaceBeforeArgument        Enable/disable spaces before the first argument (default = true)
//  [+|-]spaceBeforeColon           Enable/disable spaces before colons (default = true)
//  [+|-]spaceAfterComma            Enable/disable spaces after commas (default = true)
//  [+|-]spaceAfterSemiColon        Enable/disable spaces after semicolons (default = true)
//  [+|-]indentOnTryWith            Enable/disable indentation on try/with block (default = false)
//  [+|-]reorderOpenDeclaration     Enable/disable indentation on try/with block (default = false)

let [<Literal>] forceText = "Print the source unchanged if it cannot be parsed correctly."
let [<Literal>] recurseText = "Process the input folder recursively."
let [<Literal>] outputText = "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only."
let [<Literal>] profileText = "Print performance profiling information."

let [<Literal>] fsiText = "Read F# source from stdin as F# signatures."
let [<Literal>] stdInText = "Read F# source from standard input."
let [<Literal>] stdOutText = " Write the formatted source code to standard output."

let [<Literal>] indentText = "Set number of spaces for indentation (default = 4). The value should be between 1 and 10."
let [<Literal>] widthText = "Set the column where we break to new lines (default = 80). The value should be at least 60."

let [<Literal>] semicolonEOLText = "Enable semicolons at the end of line (default = false)."
let [<Literal>] argumentText = "Disable spaces before the first argument of functions when there are parenthesis (default = true). For methods and constructors, there are never spaces regardless of this option."
let [<Literal>] colonText = "Disable spaces before colons (default = true)."
let [<Literal>] commaText = "Disable spaces after commas (default = true)."
let [<Literal>] semicolonText = "Disable spaces after semicolons (default = true)."
let [<Literal>] indentOnTryWithText = "Enable indentation on try/with block (default = false)."
let [<Literal>] reorderOpenDeclarationText = "Enable reordering open declarations (default = false)."

let [<Literal>] spaceAroundDelimiterText = "Disable spaces after starting and before ending of lists, arrays, sequences and records (default = true)."
let [<Literal>] strictModeText = "Enable strict mode (ignoring directives and comments and printing literals in canonical forms) (default = false)."

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

let extensions = set [| ".fs"; ".fsx"; ".fsi"; ".ml"; ".mli"; |]

let isFSharpFile s = Set.contains (Path.GetExtension s) extensions

/// Get all appropriate files, either recursively or non-recursively
let rec allFiles isRec path =
    seq {
        for f in Directory.GetFiles path do
            if isFSharpFile f then yield f
        if isRec then
            for d in Directory.GetDirectories path do
                yield! allFiles isRec d
    }

/// Format a source string using given config and write to a text writer
let processSourceString isFsiFile s (tw : TextWriter) config =
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fsx"
    tw.Write(CodeFormatter.FormatDocument(fileName, s, config))

/// Format inFile and write to text writer
let processSourceFile inFile (tw : TextWriter) config = 
    let s = File.ReadAllText(inFile)
    tw.Write(CodeFormatter.FormatDocument(inFile, s, config))

[<EntryPoint>]
let main _args =
    let recurse = ref false
    let force = ref false
    let profile = ref false

    let outputPath = ref Notknown
    let inputPath = ref Unspecified

    let fsi = ref false
    let stdIn = ref false
    let stdOut = ref false
    
    let indent = ref 4
    let pageWidth = ref 80
    
    let semicolonEOL = ref false
    let spaceBeforeArgument = ref true
    let spaceBeforeColon = ref true
    let spaceAfterComma = ref true
    let spaceAfterSemiColon = ref true
    let indentOnTryWith = ref false
    let reorderOpenDeclaration = ref false

    let spaceAroundDelimiter = ref true
    let strictMode = ref false

    let handleOutput s =
        if not !stdOut then
            outputPath := IO s

    let handleStdOut() =
        stdOut := true
        outputPath := StdOut

    let handleInput s = 
        if !stdIn then
            inputPath := StdIn s
        elif Directory.Exists(s) then
           inputPath := Folder s
        elif File.Exists s && isFSharpFile s then
           inputPath := File s
        else
            eprintfn "Input path should be a file or a folder."
            exit 1

    let handleIndent i = 
        if i >= 1 && i <= 10 then
            indent := i
        else
            eprintfn "Number of spaces should be between 1 and 10."
            exit 1

    let handlePageWidth i = 
        if i >= 60 then
            pageWidth := i
        else
            eprintfn "Page width should be at least 60."
            exit 1

    let fileToFile (inFile : string) (outFile : string) config =
        try
            printfn "Processing %s" inFile
            use buffer = new StreamWriter(outFile)
            if !profile then
                File.ReadLines(inFile) |> Seq.length |> printfn "Line count: %i" 
                time (fun () -> processSourceFile inFile buffer config)
            else
                processSourceFile inFile buffer config
            buffer.Flush()
            printfn "%s has been written." outFile
        with
        | exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn
            if !force then
                File.WriteAllText (outFile, File.ReadAllText inFile)
                printfn "Force writing original contents to %s" outFile

    let fileToStdOut inFile config =
        try
            use buffer = new StringWriter()
            // Don't record running time when output formatted content to console
            processSourceFile inFile buffer config
            stdout.Write(buffer.ToString())
        with
        | exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn
            if !force then
                stdout.Write(File.ReadAllText inFile)

    let stringToFile (s : string) (outFile : string) config =
        try
            use buffer = new StreamWriter(outFile)
            if !profile then
                printfn "Line count: %i" (s.Length - s.Replace(Environment.NewLine, "").Length)
                time (fun () -> processSourceString !fsi s buffer config)
            else
                processSourceString !fsi s buffer config
            buffer.Flush()
            printfn "%s has been written." outFile
        with
        | exn ->
            eprintfn "The following exception occurs while formatting stdin: %O" exn
            if !force then
                File.WriteAllText(outFile, s)
                printfn "Force writing original contents to %s." outFile

    let stringToStdOut s config =
        try
            use buffer = new StringWriter()
            processSourceString !fsi s buffer config
            stdout.Write(buffer.ToString())
        with
        | exn ->
            eprintfn "The following exception occurs while formatting stdin: %O" exn
            if !force then
                stdout.Write(s)

    let options =
        [| ArgInfo("--recurse", ArgType.Set recurse, recurseText);
           ArgInfo("--force", ArgType.Set force, forceText);
           ArgInfo("--profile", ArgType.Set profile, profileText);

           ArgInfo("--fsi", ArgType.Set fsi, fsiText);
           ArgInfo("--stdin", ArgType.Set stdIn, stdInText);
           ArgInfo("--stdout", ArgType.Unit handleStdOut, stdOutText);
           
           // --out doesn't matter if one specifies --stdout
           ArgInfo("--out", ArgType.String handleOutput, outputText);

           ArgInfo("--indent", ArgType.Int handleIndent, indentText);
           ArgInfo("--pageWidth", ArgType.Int handlePageWidth, widthText);
           
           ArgInfo("--semicolonEOL", ArgType.Set semicolonEOL, semicolonEOLText);
           ArgInfo("--noSpaceBeforeArgument", ArgType.Clear spaceBeforeArgument, argumentText);
           ArgInfo("--noSpaceBeforeColon", ArgType.Clear spaceBeforeColon, colonText);
           ArgInfo("--noSpaceAfterComma", ArgType.Clear spaceAfterComma, commaText);
           ArgInfo("--noSpaceAfterSemiColon", ArgType.Clear spaceAfterSemiColon, semicolonText);
           ArgInfo("--indentOnTryWith", ArgType.Set indentOnTryWith, indentOnTryWithText);
           ArgInfo("--reorderOpenDeclaration", ArgType.Set reorderOpenDeclaration, reorderOpenDeclarationText);
           
           ArgInfo("--noSpaceAroundDelimiter", ArgType.Clear spaceAroundDelimiter, spaceAroundDelimiterText);
           ArgInfo("--strictMode", ArgType.Set strictMode, strictModeText) |]

    ArgParser.Parse(options, handleInput, "Fantomas <input_path>")

    let config =
        { FormatConfig.Default with 
            IndentSpaceNum = !indent;
            PageWidth = !pageWidth;
            SemicolonAtEndOfLine = !semicolonEOL; 
            SpaceBeforeArgument = !spaceBeforeArgument; 
            SpaceBeforeColon = !spaceBeforeColon;
            SpaceAfterComma = !spaceAfterComma; 
            SpaceAfterSemicolon = !spaceAfterSemiColon; 
            IndentOnTryWith = !indentOnTryWith;
            ReorderOpenDeclaration = !reorderOpenDeclaration
            SpaceAroundDelimiter = !spaceAroundDelimiter
            StrictMode = !strictMode }

    // Handle inputs via pipeline
    let isKeyAvailable = ref false

    try
        isKeyAvailable := Console.KeyAvailable
    with
    | :? InvalidOperationException ->
        // Currently only support UTF8
        Console.InputEncoding <- Text.Encoding.UTF8
        inputPath := StdIn(stdin.ReadToEnd())

    let processFile inputFile outputFile config =
        if inputFile <> outputFile then
            fileToFile inputFile outputFile config
        else
            let content = File.ReadAllText inputFile
            stringToFile content inputFile config
            
    let processFolder inputFolder outputFolder =
        if not <| Directory.Exists(outputFolder) then
            Directory.CreateDirectory(outputFolder) |> ignore
        allFiles !recurse inputFolder
        |> Seq.iter (fun i ->
            // s supposes to have form s1/suffix
            let suffix = i.Substring(inputFolder.Length + 1)
            let o =
                if inputFolder <> outputFolder then
                    Path.Combine(outputFolder, suffix)
                else i

            processFile i o config)

    match !inputPath, !outputPath with
    | Unspecified, _ ->
        eprintfn "Input path is missing."
        exit 1
    | Folder p1, Notknown -> processFolder p1 p1
    | File p1, Notknown -> processFile p1 p1 config
    | File p1, IO p2 ->
        processFile p1 p2 config
    | Folder p1, IO p2 -> processFolder p1 p2
    | StdIn s, IO p ->
        stringToFile s p config
    | StdIn s, Notknown
    | StdIn s, StdOut ->
        stringToStdOut s config
    | File p, StdOut -> 
        fileToStdOut p config
    | Folder p, StdOut ->
        allFiles !recurse p
        |> Seq.iter (fun p -> fileToStdOut p config)
    0
