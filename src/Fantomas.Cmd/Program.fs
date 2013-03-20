module Fantomas.Cmd.Program

open System
open System.IO
open Microsoft.FSharp.Text.Args

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

// Options:
//  --encoding=<encoding>               Set the encoding, e.g. UTF-8. 
//                                      If not set, defaults to the platform default encoding (currently UTF-8)
//  --force, -f                         If using --stdout, print the source unchanged if it cannot be parsed correctly
//  --help, -h                          Show help
//  --config=<path>, -c=<path>          Read preferences from a config file
//  --recurse, -r                       If any given file is a directory, recurse it and process all fs/fsx/fsi files
//  --stdin                             Read F# source from standard input
//  --stdout                            Write the formatted output to standard output

// Preferences:
//  -indentSpaceNum=[1-10]              Set number of spaces to use for indentation
//  -longIdentLength=[10-100]           The length to start breaking an expression to multiple lines
//  [+|-]semicolonAtEndOfLine           Enable/disable semicolons at the end of line (default = true)
//  [+|-]spaceBeforeArgument            Enable/disable spaces before the first argument (default = false)
//  [+|-]spaceBeforeColon               Enable/disable spaces before colons (default = true)
//  [+|-]spaceAfterComma                Enable/disable spaces after commas (default = true)
//  [+|-]spaceAfterSemiColon            Enable/disable spaces after semicolons (default = true)
//  [+|-]indentOnTryWith                Enable/disable indentation on try/with block (default = false)

let helpText = ""

let indentText = "Set number of spaces to use for indentation. The value is between 1 and 10."

let semicolonEOFText = "Enable/disable semicolons at the end of line (default = true)."
let argumentText = "Enable/disable spaces before the first argument (default = false)."
let colonText = "Enable/disable spaces before colons (default = true)."
let commaText = "Enable/disable spaces after commas (default = true)."
let semicolonText = "Enable/disable spaces after semicolons (default = true)."
let indentOnTryWithText = "Enable/disable indentation on try/with block (default = false)."

let time f =
  let sw = System.Diagnostics.Stopwatch.StartNew()
  let r = f()
  sw.Stop()
  printfn "Time taken: %O s" sw.Elapsed
  r

[<EntryPoint>]
let main args = 
    let help = ref false
    let recurse = ref false
    let force = ref false

    let outputPath = ref None
    let inputPath = ref None
    
    let indent = ref 4
    
    let semicolonAtEndOfLine = ref false
    let spaceBeforeArgument = ref false
    let spaceBeforeColon = ref true
    let spaceAfterComma = ref true
    let spaceAfterSemiColon = ref true
    let indentOnTryWith = ref false

    let options =
        [| ArgInfo("--help", ArgType.Set help, helpText);
           ArgInfo("--recursive", ArgType.Set recurse, "Process the input folder recursively.");
           ArgInfo("--force", ArgType.Set force, "Force to return original string if parsing fails.");
           ArgInfo("--out", ArgType.String(fun s -> outputPath := Some s), "Set the output path.");

           ArgInfo("--indent", ArgType.Int(fun i -> indent := i), indentText);
           
           ArgInfo("-semicolonAtEndOfLine", ArgType.Unit(fun () -> semicolonAtEndOfLine := false), semicolonEOFText);
           ArgInfo("+semicolonAtEndOfLine", ArgType.Unit(fun () -> semicolonAtEndOfLine := true), semicolonEOFText);
           ArgInfo("-spaceBeforeArgument", ArgType.Unit(fun () -> spaceBeforeArgument := false), argumentText);
           ArgInfo("+spaceBeforeArgument", ArgType.Unit(fun () -> spaceBeforeArgument := true), argumentText);
           ArgInfo("-spaceBeforeColon", ArgType.Unit(fun () -> spaceBeforeColon := false), colonText);
           ArgInfo("+spaceBeforeColon", ArgType.Unit(fun () -> spaceBeforeColon := true), colonText);
           ArgInfo("-spaceAfterComma", ArgType.Unit(fun () -> spaceAfterComma := false), commaText);
           ArgInfo("+spaceAfterComma", ArgType.Unit(fun () -> spaceAfterComma := true), commaText);
           ArgInfo("-spaceAfterSemiColon", ArgType.Unit(fun () -> spaceAfterSemiColon := false), semicolonText);
           ArgInfo("+spaceAfterSemiColon", ArgType.Unit(fun () -> spaceAfterSemiColon := true), semicolonText);
           ArgInfo("-indentOnTryWith", ArgType.Unit(fun () -> indentOnTryWith := false), indentOnTryWithText);
           ArgInfo("+indentOnTryWith", ArgType.Unit(fun () -> indentOnTryWith := true), indentOnTryWithText); |]

    let parseInput s = 
        match !inputPath with
        | None -> inputPath := Some s
        | Some _ -> printfn "Input path has already been specified."

    ArgParser.Parse(options, parseInput, "Fantomas.Cmd <filename>")

    // Assume that we get the input as a single file

    if Option.isNone !outputPath then
        outputPath := !inputPath 
                      |> Option.map (fun s ->
                            let name = Path.GetFileNameWithoutExtension(s) + "_out"
                            let ext = Path.GetExtension(s)
                            Path.Combine(Path.GetDirectoryName(s), name + ext))

    let config = { FormatConfig.Default with 
                    IndentSpaceNum = !indent;
                    SemicolonAtEndOfLine = !semicolonAtEndOfLine; 
                    SpaceBeforeArgument = !spaceBeforeArgument; 
                    SpaceBeforeColon = !spaceBeforeColon;
                    SpaceAfterComma = !spaceAfterComma; 
                    SpaceAfterSemicolon = !spaceAfterSemiColon; 
                    IndentOnTryWith = !indentOnTryWith }
    
    match !inputPath, !outputPath with
    | Some s1, Some s2 -> 
        time (fun () -> processSourceFile s1 s2 config)
        Console.WriteLine("{0} has been written.", s2)
    | _ -> ()

    Console.WriteLine("Press any key to finish...")
    Console.ReadKey() |> ignore
    0
