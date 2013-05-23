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

/// Convert from range to string positions
let internal stringPos (r : range) (content : string) =
    let positions = 
        content.Split([|'\n'|], StringSplitOptions.None)
        |> Seq.map (fun s -> String.length s + 1)
        |> Seq.scan (+) 0
        |> Seq.toArray

    let start = positions.[r.StartLine-1] + r.StartColumn
    let finish = positions.[r.EndLine-1] + r.EndColumn
    (start, finish)

/// Make a range from (startLine, startCol) to (endLine, endCol) for selecting some text
let makeRange startLine startCol endLine endCol =
    mkRange "/tmp.fs" (mkPos startLine startCol) (mkPos endLine endCol)

/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString fsi (r : range) (s : string) config =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    let sourceTok = SourceTokenizer([], fileName)

    /// Move to the section with real contents
    let r =
        if r.StartLine = r.EndLine then r
        else
            /// Get first non-whitespace line
            let rec getStartLine i =
                if i = lines.Length-1 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
                else getStartLine(i + 1)
            let startLine = getStartLine(r.StartLine - 1)

            let rec getEndLine i =
                if i = 0 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
                else getEndLine(i - 1)
            let endLine = getEndLine(r.EndLine - 1) 
            /// Notice that Line indices start at 1 while Column indices start at 0.
            makeRange (startLine + 1) 0 (endLine + 1) (lines.[endLine].Length-1)

    let startTokenizer = sourceTok.CreateLineTokenizer(lines.[r.StartLine-1])

    let isStartToken (tok : TokenInformation) =
        tok.CharClass <> TokenCharKind.WhiteSpace && 
        tok.CharClass <> TokenCharKind.LineComment &&
        tok.CharClass <> TokenCharKind.Comment

    /// Find out the starting token
    let rec getStartCol (tokenizer : LineTokenizer) nstate = 
        match tokenizer.ScanToken(!nstate) with
        | Some(tok), state ->
            if tok.RightColumn >= r.StartColumn && isStartToken tok then tok.LeftColumn
            else
                nstate := state 
                getStartCol tokenizer nstate
        | None, _ -> r.StartColumn 
    let startCol = getStartCol startTokenizer (ref 0L)

    let endTokenizer =
        if r.StartLine = r.EndLine then startTokenizer 
        else sourceTok.CreateLineTokenizer(lines.[r.EndLine-1])

    /// Find out the ending token
    let rec getEndCol (tokenizer : LineTokenizer) nstate = 
        match tokenizer.ScanToken(!nstate) with
        | Some(tok), state ->

#if DEBUG
            printfn "End token: %A" tok
#endif

            if tok.RightColumn >= r.EndColumn then tok.RightColumn
            else
                nstate := state 
                getEndCol tokenizer nstate
        | None, _ -> r.EndColumn 
    let endCol = getEndCol endTokenizer (ref 0L)
    
    let range = makeRange r.StartLine startCol r.EndLine endCol
    let (start, finish) = stringPos range s
    let pre = s.[0..start-1]

    /// Patch selection by an appropriate amount of whitespace
    let selection = 
        let sel = s.[start..finish]
        if r.StartLine = r.EndLine then sel
        else (new String(' ', startCol)) + sel

    let post = 
        if finish + 1 < s.Length && s.[finish + 1] = '\n' then "\r" + s.[finish + 1..] 
        elif finish + 1 < s.Length then s.[finish + 1..]
        else ""

#if DEBUG
    printfn "pre:\n%O" pre
    printfn "selection:\n%O" selection
    printfn "post:\n%O" post
#endif

    let tree = parse fsi selection

    Context.createContext config selection 
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

