module Fantomas.CodeFormatter

open System
open System.IO
open System.Diagnostics
open System.Text.RegularExpressions

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fantomas.TokenMatcher
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
    Context.create config s 
    |> genParsedInput (parse fsi s) 
    |> dump
    |> integrateComments s

/// Format a source string using given config; return None if failed
let tryFormatSourceString fsi s config =
    try
        Some (formatSourceString fsi s config)
    with 
    _ -> None

/// Format a source string using given config and write to a text writer
let processSourceString fsi s (tw : TextWriter) config =
    tw.Write(formatSourceString fsi s config)

/// Format a source string using given config and write to a text writer; return None if failed
let tryProcessSourceString fsi s tw config =
    try
        Some (processSourceString fsi s tw config)
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
    // We can't assume the range is valid, so check string boundary here
    let finish = 
        let pos = positions.[r.EndLine-1] + r.EndColumn
        if pos >= content.Length then content.Length - 1 else pos 
    (start, finish)

/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange startLine startCol endLine endCol =
    mkRange "/tmp.fs" (mkPos startLine startCol) (mkPos endLine endCol)

/// Get first non-whitespace line
let rec internal getStartLine (lines : _ []) i =
    if i = lines.Length-1 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getStartLine lines (i + 1)

let rec internal getEndLine (lines : _ []) i =
    if i = 0 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getEndLine lines (i - 1)

let internal isDelimitToken (tok : TokenInformation) =
        tok.CharClass <> TokenCharKind.WhiteSpace && 
        tok.CharClass <> TokenCharKind.LineComment &&
        tok.CharClass <> TokenCharKind.Comment &&
        tok.TokenName <> "STRING_TEXT"

/// Find out the start token
let rec internal getStartCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
        if tok.RightColumn >= r.StartColumn && isDelimitToken tok then tok.LeftColumn
        else
            nstate := state 
            getStartCol r tokenizer nstate
    | None, _ -> r.StartColumn 

/// Find out the end token
let rec internal getEndCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
        Debug.WriteLine("End token: {0}", sprintf "%A" tok)
        if tok.RightColumn >= r.EndColumn && isDelimitToken tok then tok.RightColumn
        else
            nstate := state 
            getEndCol r tokenizer nstate
    | None, _ -> r.EndColumn 

type internal Patch =
    | TypeMember
    | RecType
    | RecLet
    | NoPatch

let internal startWithMember (sel : string) =  
    [|"member"; "abstract"; "default"; "override"; 
      "static"; "interface"; "new"; "val"; "inherit"|] 
    |> Array.exists sel.StartsWith 

/// Find the first type declaration or let binding at beginnings of lines
let internal getPatch startCol (lines : string []) =
    let rec loop i = 
        if i < 0 then NoPatch 
        elif Regex.Match(lines.[i], "^[\s]*type ").Success then RecType
        else
            // Need to compare column to ensure that the let binding is at the same level
            let m = Regex.Match(lines.[i], "^[\s]*let ")
            let col = m.Index + m.Length
            // Value 4 accounts for length of "and "
            if m.Success && col <= startCol + 4 then RecLet else loop (i - 1)
    loop (lines.Length - 1)

/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString fsi (r : range) (s : string) config =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let sourceToken = SourceTokenizer([], "/tmp.fsx")

    // Move to the section with real contents
    let r =
        if r.StartLine = r.EndLine then r
        else
            let startLine = getStartLine lines (r.StartLine - 1)
            let endLine = getEndLine lines (r.EndLine - 1) 
            // Notice that Line indices start at 1 while Column indices start at 0.
            makeRange (startLine + 1) 0 (endLine + 1) (lines.[endLine].Length - 1)

    let startTokenizer = sourceToken.CreateLineTokenizer(lines.[r.StartLine-1])

    let startCol = getStartCol r startTokenizer (ref 0L)

    let endTokenizer =
        if r.StartLine = r.EndLine then startTokenizer 
        else sourceToken.CreateLineTokenizer(lines.[r.EndLine-1])

    let endCol = getEndCol r endTokenizer (ref 0L)
    
    let range = makeRange r.StartLine startCol r.EndLine endCol
    let (start, finish) = stringPos range s
    let pre = if start = 0 then "" else s.[0..start-1]

    // Prepend selection by an appropriate amount of whitespace
    let (selection, patch) = 
        let sel = s.[start..finish]
        if startWithMember sel then
           (sprintf "type T = \n%s" (new String(' ', startCol) + sel), TypeMember)
        elif sel.StartsWith("and") then
            let p = getPatch startCol lines.[..r.StartLine - 1]
            let pattern = Regex("and")
            let replacement = 
                match p with
                | RecType -> "type"
                | RecLet -> "let rec"
                | _ -> "and"
            // Replace "and" by "type" or "let rec"
            if r.StartLine = r.EndLine then (pattern.Replace(sel, replacement, 1), p)
            else (new String(' ', startCol) + pattern.Replace(sel, replacement, 1), RecType)
        elif r.StartLine = r.EndLine then (sel, NoPatch)
        else (new String(' ', startCol) + sel, NoPatch)

    let post = 
        if finish + 1 < s.Length && s.[finish + 1] = '\n' then "\r" + s.[finish + 1..] 
        elif finish + 1 < s.Length then s.[finish + 1..]
        else ""

    Debug.WriteLine("pre:\n{0}", pre)
    Debug.WriteLine("selection:\n{0}", selection)
    Debug.WriteLine("post:\n{0}", post)

    let formatSelection fsi config selection =
        Context.create config selection
        |> genParsedInput (parse fsi selection)
        |> ifElse (s.[finish] = '\n') sepNln sepNone
        |> dump
        |> integrateComments selection

    let reconstructSourceCode startCol formatteds pre post =
        // Realign results on the correct column
        Context.create config "" 
        |> str pre
        |> atIndentLevel startCol (col sepNln formatteds str)
        |> str post
        |> dump

    match patch with
    | TypeMember ->
        // Get formatted selection with "type T = \n" patch
        let result = formatSelection fsi config selection
        // Remove the patch
        let contents = result.Replace("\r\n","\n").Split('\r', '\n')
        if contents = [||] then
            sprintf "%s%s%s" pre result post
        else
            // Due to patching, the text has at least two lines
            let first = contents.[1]
            let column = first.Length - first.TrimStart().Length
            let formatteds = contents.[1..] |> Seq.map (fun s -> s.[column..])
            reconstructSourceCode startCol formatteds pre post
    | RecType 
    | RecLet ->        
        // Get formatted selection with "type" or "let rec" replacement for "and"
        let result = formatSelection fsi config selection
        // Substitute by old contents
        let pattern = if patch = RecType then Regex("type") else Regex("let rec")
        let formatteds = pattern.Replace(result, "and", 1).Replace("\r\n","\n").Split('\r', '\n')
        reconstructSourceCode startCol formatteds pre post
    | NoPatch ->
        let result = formatSelection fsi config selection
        let formatteds = result.Replace("\r\n","\n").Split('\r', '\n')
        reconstructSourceCode startCol formatteds pre post

/// Format selection in range r and keep other parts unchanged; return None if failed
let tryFormatSelectionFromString fsi (r : range) (s : string) config =
    try
        Some (formatSelectionFromString fsi r s config)
    with 
    _ -> None

