module Fantomas.CodeFormatter

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fantomas.FormatConfig
open Fantomas.SourceParser
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
let parse fsi s = 
    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    parseWith fileName s

/// Format a source string using given config
let formatSourceString fsi s config =
    Context.createContext config s 
    |> genParsedInput (parse fsi s) 
    |> dump

/// Format a source string using given config; return None if failed
let tryFormatSourceString fsi s config =
    try
        Some (formatSourceString fsi s config)
    with 
    _ -> None

/// Format a source string using given config and write to a text writer
let processSourceString fsi inStr (tw : TextWriter) config =
    Context.createContext config inStr 
    |> genParsedInput (parse fsi inStr) 
    |> dump
    |> tw.Write

/// Format a source string using given config and write to a text writer; return None if failed
let tryProcessSourceString fsi inStr tw config =
    try
        Some (processSourceString fsi inStr tw config)
    with 
    _ -> None

/// Format inFile and write to text writer
let processSourceFile inFile (tw : TextWriter) config = 
    let s = File.ReadAllText(inFile)
    let fsi = inFile.EndsWith(".fsi") || inFile.EndsWith(".mli")
    tw.Write(formatSourceString fsi s config)

/// Format inFile and write to text writer; return None if failed
let tryProcessSourceFile inFile tw config = 
    try
        Some (processSourceFile inFile tw config)
    with 
    _ -> None

/// Format a part of source string using given config; other parts are kept unchanged. 
/// Notice that Line indices start at 1
/// There is an offset -1 on EndColumn
let formatSelectionFromString fsi (r : range) (s : string) config =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)
    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    let sourceTok = SourceTokenizer([], fileName)

    let tokenizer = sourceTok.CreateLineTokenizer(lines.[r.StartLine-1])
    /// Find out the starting token
    let rec getStartCol (tokenizer : LineTokenizer) nstate = 
        match tokenizer.ScanToken(!nstate) with
        | Some(tok), state ->
            if tok.LeftColumn <= r.StartColumn && tok.RightColumn > r.StartColumn 
            then tok.LeftColumn
            else
                nstate := state 
                getStartCol tokenizer nstate
        | None, _ -> r.StartColumn 
    let startCol = getStartCol tokenizer (ref 0L)

    let tokenizer =
        if r.StartLine = r.EndLine 
        then tokenizer 
        else sourceTok.CreateLineTokenizer(lines.[r.EndLine-1])

    /// Find out the ending token
    let rec getEndCol (tokenizer : LineTokenizer) nstate = 
        match tokenizer.ScanToken(!nstate) with
        | Some(tok), state ->
            if tok.LeftColumn < r.EndColumn && tok.RightColumn >= r.EndColumn 
            then tok.RightColumn
            else
                nstate := state 
                getEndCol tokenizer nstate
        | None, _ -> r.EndColumn 
    let endCol = getEndCol tokenizer (ref 0L)

    let context = Context.createContext config s
    let range = mkRange fileName (mkPos r.StartLine startCol) (mkPos r.EndLine endCol)
    let (start, finish) = stringPos range context
    let pre = s.[0..start-1]
    let selection = s.[start..finish]
    let post = s.[finish+1..]

    printfn "pre: %O" pre
    printfn "selection: %O" selection
    printfn "post: %O" post

    let tree = parse fsi selection
    context 
    |> str pre
    |> atIndentLevel startCol (genParsedInput tree)
    |> ifElse (s.[finish] = '\n') sepNln sepNone
    |> str post
    |> dump

/// Format selection in range r and keep other parts unchanged; return None if failed
let tryFormatSelectionFromString fsi (r : range) (s : string) config =
    try
        Some (formatSelectionFromString fsi r s config)
    with 
    _ -> None

