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

let tryF f x = try Some (f x) with _ -> None

/// Parse a source code string
let parse fsi s = 
    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    parseWith fileName s

let pDumpTree fsi inStr config = 
    (parse fsi inStr, Context.createContext config inStr)
    ||> genParsedInput
    |>  dump

/// Format a source string using given config
let formatSourceString fsi s config =
    pDumpTree fsi s config

/// Format a source string using given config; return None if failed
let tryFormatSourceString fsi s config =
    tryF (formatSourceString fsi s) config

/// Format a source string using given config and write to a text writer
let processSourceString fsi s config (tw : TextWriter)  =
   pDumpTree fsi s config
   |> tw.Write

/// Format a source string using given config and write to a text writer; return None if failed
let tryProcessSourceString fsi inStr tw config =
    tryF (processSourceString fsi inStr tw) config

/// Format inFile and write to text writer
let processSourceFile inFile (tw : TextWriter) config = 
    let s = File.ReadAllText inFile
    let fsi = inFile.EndsWith ".fsi" || inFile.EndsWith ".mli"
    tw.Write(formatSourceString fsi s config)

/// Format inFile and write to text writer; return None if failed
let tryProcessSourceFile inFile tw config = 
    tryF (processSourceFile inFile tw) config
