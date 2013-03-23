module Fantomas.Cmd.Program

open System
open System.IO
open Microsoft.FSharp.Text.Args

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

/// Options:
///  --encoding=<encoding>           Set the encoding, e.g. UTF-8. 
///                                  If not set, defaults to the platform default encoding (currently UTF-8)
///  --force                         If using --stdout, print the source unchanged if it cannot be parsed correctly
///  --help                          Show help
///  --recurse                       If any given file is a directory, recurse it and process all fs/fsx/fsi files
///  --stdin                         Read F# source from standard input
///  --stdout                        Write the formatted output to standard output

/// Preferences:
///  --indent=[1-10]                 Set number of spaces to use for indentation
///  -longIdent=[10-100]             The length to start breaking an expression to multiple lines
///  [+|-]semicolonEOL       Enable/disable semicolons at the end of line (default = true)
///  [+|-]spaceBeforeArgument        Enable/disable spaces before the first argument (default = false)
///  [+|-]spaceBeforeColon           Enable/disable spaces before colons (default = true)
///  [+|-]spaceAfterComma            Enable/disable spaces after commas (default = true)
///  [+|-]spaceAfterSemiColon        Enable/disable spaces after semicolons (default = true)
///  [+|-]indentOnTryWith            Enable/disable indentation on try/with block (default = false)

let indentText = "Set number of spaces for indentation (default = 4). The value is between 1 and 10."

let semicolonEOLText = "Disable semicolons at the end of line (default = true)."
let argumentText = "Enable spaces before the first argument (default = false)."
let colonText = "Disable spaces before colons (default = true)."
let commaText = "Disable spaces after commas (default = true)."
let semicolonText = "Disable spaces after semicolons (default = true)."
let indentOnTryWithText = "Enable indentation on try/with block (default = false)."

let forceText = "Force returning the original string if parsing fails."
let recurseText = "Process the input folder recursively."
let outputText = "Give a valid path for files/folders. Files should have .fs, .fsx, .fsi, .ml or .mli extension only."

let time f =
  let sw = System.Diagnostics.Stopwatch.StartNew()
  let r = f()
  sw.Stop()
  printfn "Time taken: %O s" sw.Elapsed
  r

type PathParam = 
    | File of string 
    | Folder of string 
    | Nothing

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

    let outputPath = ref Nothing
    let inputPath = ref Nothing
    
    let indent = ref 4
    
    let semicolonEOL = ref true
    let spaceBeforeArgument = ref false
    let spaceBeforeColon = ref true
    let spaceAfterComma = ref true
    let spaceAfterSemiColon = ref true
    let indentOnTryWith = ref false

    let handleOutput s =
        if Directory.Exists(s) then
           outputPath := Folder s
        elif File.Exists(s) && isFSharpFile(s) then
           outputPath := File s
        else
            Console.WriteLine("Output path should be a file or a folder.")
            exit 1

    let handleInput s = 
        if Directory.Exists(s) then
           inputPath := Folder s
        elif File.Exists(s) && isFSharpFile(s) then
           inputPath := File s
        else
            Console.WriteLine("Input path should be a file or a folder.")
            exit 1

    let handleIndent i = 
        if i >= 1 && i <= 10 then
            indent := i
        else
            Console.WriteLine("Number of spaces should be between 1 and 10.")
            exit 1

    let processSourceCode inFile outFile config =
        try
            time (fun () -> processSourceFile inFile outFile config)
            Console.WriteLine("{0} has been written.", outFile)
        with
        | _ ->
            if !force then
                File.WriteAllText(outFile, File.ReadAllText(inFile))
                Console.WriteLine("Forced writing original contents to {0}.", outFile)

    let options =
        [| ArgInfo("--recurse", ArgType.Set recurse, recurseText);
           ArgInfo("--force", ArgType.Set force, forceText);
           ArgInfo("--out", ArgType.String handleOutput, outputText);

           ArgInfo("--indent", ArgType.Int handleIndent, indentText);
           
           ArgInfo("--noSemicolonEOL", ArgType.Clear semicolonEOL, semicolonEOLText);
           ArgInfo("--spaceBeforeArgument", ArgType.Set spaceBeforeArgument, argumentText);           
           ArgInfo("--noSpaceBeforeColon", ArgType.Clear spaceBeforeColon, colonText);
           ArgInfo("--noSpaceAfterComma", ArgType.Clear spaceAfterComma, commaText);
           ArgInfo("--noSpaceAfterSemiColon", ArgType.Clear spaceAfterSemiColon, semicolonText);
           ArgInfo("--indentOnTryWith", ArgType.Set indentOnTryWith, indentOnTryWithText); |]

    ArgParser.Parse(options, handleInput, "Fantomas.Cmd <input_path>")

    let config = { FormatConfig.Default with 
                    IndentSpaceNum = !indent;
                    SemicolonAtEndOfLine = !semicolonEOL; 
                    SpaceBeforeArgument = !spaceBeforeArgument; 
                    SpaceBeforeColon = !spaceBeforeColon;
                    SpaceAfterComma = !spaceAfterComma; 
                    SpaceAfterSemicolon = !spaceAfterSemiColon; 
                    IndentOnTryWith = !indentOnTryWith }

    match !inputPath, !outputPath with
    | Nothing, _ -> 
        Console.WriteLine("Input path is missing.")
        exit 1
    | _, Nothing ->
        Console.WriteLine("Output path is missing.")
        exit 1
    | File s1, File s2 ->
        processSourceCode s1 s2 config
    | File _, Folder _ ->
        Console.WriteLine("Output path should be a file since input path is.")
        exit 1
    | Folder _, File _ ->
        Console.WriteLine("Output path should be a folder since input path is.")
        exit 1
    | Folder s1, Folder s2 ->
        allFiles !recurse s1
        |> Seq.iter (fun s ->     
            /// s supposes to have form s1/suffix
            let suffix = s.Substring(s1.Length + 1)
            let o = Path.Combine(s2, suffix)
            processSourceCode s o config)
    0
