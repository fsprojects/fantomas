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
    /// We can't assume the range is valid, so check string boundary here
    let finish = 
        let pos = positions.[r.EndLine-1] + r.EndColumn
        if pos >= content.Length then content.Length - 1 else pos 
    (start, finish)

/// Make a range from (startLine, startCol) to (endLine, endCol) for selecting some text
let makeRange startLine startCol endLine endCol =
    mkRange "/tmp.fs" (mkPos startLine startCol) (mkPos endLine endCol)

/// Get first non-whitespace line
let rec internal getStartLine (lines : _ []) i =
    if i = lines.Length-1 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getStartLine lines (i + 1)

let rec internal getEndLine (lines : _ []) i =
    if i = 0 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getEndLine lines (i - 1)

let internal isStartToken (tok : TokenInformation) =
        tok.CharClass <> TokenCharKind.WhiteSpace && 
        tok.CharClass <> TokenCharKind.LineComment &&
        tok.CharClass <> TokenCharKind.Comment

/// Find out the start token
let rec internal getStartCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
        if tok.RightColumn >= r.StartColumn && isStartToken tok then tok.LeftColumn
        else
            nstate := state 
            getStartCol r tokenizer nstate
    | None, _ -> r.StartColumn 

/// Find out the end token
let rec internal getEndCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
#if DEBUG
        printfn "End token: %A" tok
#endif
        if tok.RightColumn >= r.EndColumn then tok.RightColumn
        else
            nstate := state 
            getEndCol r tokenizer nstate
    | None, _ -> r.EndColumn 

type internal Patch =
    | TypeMember
    | RecType
    | RecLet
    | NoPatch

/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString fsi (r : range) (s : string) config =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let fileName = if fsi then "/tmp.fsi" else "/tmp.fs"
    let sourceTok = SourceTokenizer([], fileName)

    /// Move to the section with real contents
    let r =
        if r.StartLine = r.EndLine then r
        else
            let startLine = getStartLine lines (r.StartLine - 1)
            let endLine = getEndLine lines (r.EndLine - 1) 
            /// Notice that Line indices start at 1 while Column indices start at 0.
            makeRange (startLine + 1) 0 (endLine + 1) (lines.[endLine].Length - 1)

    let startTokenizer = sourceTok.CreateLineTokenizer(lines.[r.StartLine-1])

    let startCol = getStartCol r startTokenizer (ref 0L)

    let endTokenizer =
        if r.StartLine = r.EndLine then startTokenizer 
        else sourceTok.CreateLineTokenizer(lines.[r.EndLine-1])

    let endCol = getEndCol r endTokenizer (ref 0L)
    
    let range = makeRange r.StartLine startCol r.EndLine endCol
    let (start, finish) = stringPos range s
    let pre = if start = 0 then "" else s.[0..start-1]

    let isTypeMember (sel : string) =   
        Array.exists sel.StartsWith [|"member"; "abstract"; "default"; "override"; "static"; "interface"|]

    /// Prepend selection by an appropriate amount of whitespace
    let (selection, patch) = 
        let sel = s.[start..finish]
        if isTypeMember sel then
           (sprintf "type T() = \n%s" (new String(' ', startCol) + sel), TypeMember)
        elif r.StartLine = r.EndLine then (sel, NoPatch)
        else (new String(' ', startCol) + sel, NoPatch)

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

    match patch with
    | TypeMember ->
        /// Get formatted selection with "type T() = \n" patch
        let result = 
            Context.createContext config selection
            |> genParsedInput tree
            |> ifElse (s.[finish] = '\n') sepNln sepNone
            |> dump
        /// Remove the patch
        let contents = result.Split([|'\n'|], StringSplitOptions.None)
        if contents = [||] then
            sprintf "%s%s%s" pre result post
        else
            let first = contents.[1]
            let column = first.Length - first.TrimStart().Length
            let selections = contents.[1..] |> Seq.map (fun s -> s.[column..])
            /// Realign results on the correct column
            Context.createContext config "" 
            |> str pre
            |> atIndentLevel startCol (col sepNln selections str)
            |> ifElse (s.[finish] = '\n') sepNln sepNone
            |> str post
            |> dump
    | RecType
    | RecLet
    | NoPatch ->
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

