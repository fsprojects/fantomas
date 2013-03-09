module Fantomas.CodeFormatter

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fantomas.FormatConfig
open Fantomas.CodePrinter

let internal parseWith fileName content = 
    // Create an interactive checker instance (ignore notifications)
    let checker = InteractiveChecker.Create(NotifyFileTypeCheckStateIsDirty ignore)
    // Get compiler options for a single script file
    let checkOptions = checker.GetCheckOptionsFromScriptRoot(fileName, content, DateTime.Now, [||])
    // Run the first phase (untyped parsing) of the compiler
    let untypedRes = checker.UntypedParse(fileName, content, checkOptions)
    match untypedRes.ParseTree with
    | Some tree -> tree
    | None -> failwith "parseWith: Unexpected input"

/// Parse a source code string
let parse s = parseWith "/var/tmp.fs" s
           
/// Format a source code tree using config
let format tree config = 
    genParsedInput tree { Context.Default with Config = config } |> dump

/// Format a source file using given config
let formatFile f config = 
    let s = File.ReadAllText(f)
    let tree = parseWith f s
    format tree config

/// Format a source string using given config
let formatString s config =
    let tree = parseWith "/var/tmp.fs" s
    format tree config

/// Format inFile and write to outFile
let processFile inFile outFile config = 
    let s = formatFile inFile config
    File.WriteAllText(outFile, s)
    
