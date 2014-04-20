module Fantomas.CodeFormatter

open System
open System.Diagnostics
open System.Collections.Generic
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
    let checker = InteractiveChecker.Create()
    // Get compiler options for a single script file
    let checkOptions = 
        checker.GetProjectOptionsFromScript(fileName, content, DateTime.Now, filterDefines content) 
        |> Async.RunSynchronously
    // Run the first phase (untyped parsing) of the compiler
    let untypedRes = checker.ParseFileInProject(fileName, content, checkOptions) |> Async.RunSynchronously
    match untypedRes.ParseTree with
    | Some tree -> tree
    | None -> raise <| FormatException "Parsing failed. Please select a complete code fragment to format."

/// Parse a source code string
let parse isFsiFile s = 
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fs"
    parseWith fileName s

let internal split (s : string) =
    s.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')

let internal formatWith ast (s : string) config =
    // Use '\n' as the new line delimiter consistently
    // It would be easier for F# parser
    let s = s.Replace("\r\n", "\n").Replace("\r", "\n")
    let s' =    
        Context.create config s 
        |> genParsedInput ASTContext.Default ast
        |> dump
        |> if config.StrictMode then id else integrateComments s

    // Sometimes F# parser gives a partial AST for incorrect input
    if String.IsNullOrWhiteSpace s <> String.IsNullOrWhiteSpace s' then
        raise <| FormatException """Incomplete code fragment. 
This is mostly because of parsing errors or the use of F# constructs newer than being supported."""
    else s'

let internal format isFsiFile (s : string) config =
    formatWith (parse isFsiFile s) s config

/// Format a source string using given config
let formatSourceString isFsiFile s config =    
    let s' = format isFsiFile s config
        
    // When formatting the whole document, an EOL is required
    if s'.EndsWith(Environment.NewLine) then s' else s' + Environment.NewLine

/// Format an abstract syntax tree using given config
let formatAST ast s config =    
    let s' = formatWith ast s config
        
    // When formatting the whole document, an EOL is required
    if s'.EndsWith(Environment.NewLine) then s' else s' + Environment.NewLine

/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange startLine startCol endLine endCol =
    mkRange "/tmp.fsx" (mkPos startLine startCol) (mkPos endLine endCol)

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
        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
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
    |> Array.exists (sel.TrimStart().StartsWith)

/// Find the first type declaration or let binding at beginnings of lines
let internal getPatch startCol (lines : string []) =
    let rec loop i = 
        if i < 0 then NoPatch 
        elif Regex.Match(lines.[i], "^[\s]*type").Success then RecType
        else
            // Need to compare column to ensure that the let binding is at the same level
            let m = Regex.Match(lines.[i], "^[\s]*let")
            let col = m.Index + m.Length
            // Value 4 accounts for length of "and "
            if m.Success && col <= startCol + 4 then RecLet else loop (i - 1)
    loop (lines.Length - 1)

let internal formatRange replaceDocument isFsiFile startLine startCol endLine endCol (lines : _ []) (s : string) config =
    let s = s.Replace("\r\n", "\n").Replace("\r", "\n")
    // Convert from range to string positions
    let stringPos (r : range) =
        // Assume that content has been normalized (no "\r\n" anymore)
        let positions = 
            s.Split('\n')
            |> Seq.map (fun s -> String.length s + 1)
            |> Seq.scan (+) 0
            |> Seq.toArray

        let start = positions.[r.StartLine-1] + r.StartColumn
        // We can't assume the range is valid, so check string boundary here
        let finish = 
            let pos = positions.[r.EndLine-1] + r.EndColumn
            if pos >= s.Length then s.Length - 1 else pos 
        (start, finish)

    let range = makeRange startLine startCol endLine endCol
    let (start, finish) = stringPos range
    let pre = if start = 0 || not replaceDocument then "" else s.[0..start-1]

    // Prepend selection by an appropriate amount of whitespace
    let (selection, patch) = 
        let sel = s.[start..finish]
        if startWithMember sel then
           (String.Join("", "type T = ", Environment.NewLine, new String(' ', startCol), sel), TypeMember)
        elif sel.TrimStart().StartsWith("and") then
            let p = getPatch startCol lines.[..startLine-1]
            let pattern = Regex("and")
            let replacement = 
                match p with
                | RecType -> "type"
                | RecLet -> "let rec"
                | _ -> "and"
            // Replace "and" by "type" or "let rec"
            if startLine = endLine then (pattern.Replace(sel, replacement, 1), p)
            else (new String(' ', startCol) + pattern.Replace(sel, replacement, 1), p)
        elif startLine = endLine then (sel, NoPatch)
        else (new String(' ', startCol) + sel, NoPatch)

    let post =
        if finish < s.Length && replaceDocument then 
            s.[finish+1..].Replace("\n", Environment.NewLine)
        else ""

    Debug.WriteLine("pre:\n{0}", box pre)
    Debug.WriteLine("selection:\n{0}", box selection)
    Debug.WriteLine("post:\n{0}", box post)

    let formatSelection isFsiFile s config =
        let s' = format isFsiFile s config
        // If the input is not inline, the output should not be inline as well
        if s.EndsWith("\n") && not <| s'.EndsWith(Environment.NewLine) then 
            s' + Environment.NewLine 
        else s'

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
        let result = formatSelection isFsiFile selection config
        // Remove the patch
        let contents = split result
        if Array.isEmpty contents then
            String.Join("", pre, result, post)
        else
            // Due to patching, the text has at least two lines
            let first = contents.[1]
            let column = first.Length - first.TrimStart().Length
            let formatteds = contents.[1..] |> Seq.map (fun s -> s.[column..])
            reconstructSourceCode startCol formatteds pre post
    | RecType 
    | RecLet ->        
        // Get formatted selection with "type" or "let rec" replacement for "and"
        let result = formatSelection isFsiFile selection config
        // Substitute by old contents
        let pattern = if patch = RecType then Regex("type") else Regex("let rec")
        let formatteds = split (pattern.Replace(result, "and", 1))
        reconstructSourceCode startCol formatteds pre post
    | NoPatch ->
        let result = formatSelection isFsiFile selection config
        let formatteds = split result
        reconstructSourceCode startCol formatteds pre post

/// Format a part of source string using given config, and return the (formatted) selected part only.
let formatSelectionOnly isFsiFile (r : range) (s : string) config =
    let lines = split s
    formatRange false isFsiFile r.StartLine r.StartColumn r.EndLine r.EndColumn lines s config
    
/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString isFsiFile (r : range) (s : string) config =
    let lines = split s
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
    formatRange true isFsiFile r.StartLine startCol r.EndLine endCol lines s config    

type internal BlockType =
   | List
   | Array
   | SequenceOrRecord
   | Tuple

/// Make a position from (line, col) to to denote cursor position
let makePos line col = mkPos line col

/// Format around cursor delimited by '[' and ']', '{' and '}' or '(' and ')' using given config; keep other parts unchanged. 
let formatAroundCursor isFsiFile (p : pos) (s : string) config = 
    let lines = split s
    let sourceTokenizer = SourceTokenizer([], "/tmp.fsx")
    let openDelimiters = dict ["[", List; "[|", Array; "{", SequenceOrRecord; "(", Tuple]
    let closeDelimiters = dict ["]", List; "|]", Array; "}", SequenceOrRecord; ")", Tuple]

    /// Find the delimiter at the end
    let rec tryFindEndDelimiter (dic : Dictionary<_, _>) i (lines : _ []) =
        if i >= lines.Length then
            None
        else
            let line = lines.[i]
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let finLine = ref false
            let result = ref None
            let lexState = ref 0L
            while not !finLine do
                let tok, newLexState = lineTokenizer.ScanToken(!lexState)
                lexState := newLexState
                match tok with 
                | None -> 
                    finLine := true
                | Some t when t.CharClass = TokenCharKind.Delimiter -> 
                    if i + 1 > p.Line || (i + 1 = p.Line && t.RightColumn >= p.Column) then
                        let text = line.[t.LeftColumn..t.RightColumn]
                        match text with
                        | "[" | "[|" | "{" | "(" ->
                            Debug.WriteLine("Found opening token '{0}'", text)
                            let delimiter = openDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, c -> 
                                dic.[delimiter] <- c + 1
                            | _ -> 
                                dic.Add(delimiter, 1)
                        | "]" | "|]" | "}" | ")" ->
                            Debug.WriteLine("Found closing token '{0}'", text)
                            let delimiter = closeDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, 1 -> 
                                dic.Remove(delimiter) |> ignore
                            | true, c -> 
                                dic.[delimiter] <- c - 1
                            | _ -> 
                                // The delimiter has count 0; record as a result
                                Debug.WriteLine("Record closing token '{0}'", text)
                                result := Some (i + 1, t.RightColumn, delimiter)
                        | _ -> ()
                | _ -> ()

            if Option.isNone !result then
                tryFindEndDelimiter dic (i + 1) lines
            else
                !result

    /// Find the delimiter at the beginning              
    let rec tryFindStartDelimiter blockType (dic : Dictionary<_, _>) acc i (lines : _ []) =
        if i >= p.Line then
            acc
        else
            let line = lines.[i]
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let finLine = ref false
            let result = ref acc
            let lexState = ref 0L
            while not !finLine do
                let tok, newLexState = lineTokenizer.ScanToken(!lexState)
                lexState := newLexState
                match tok with 
                | None -> 
                    finLine := true
                | Some t when t.CharClass = TokenCharKind.Delimiter -> 
                    if i + 1 < p.Line || (i + 1 = p.Line && t.LeftColumn <= p.Column) then
                        let text = line.[t.LeftColumn..t.RightColumn]
                        match text, blockType with
                        | "]", List
                        | "|]", Array 
                        | "}", SequenceOrRecord 
                        | ")", Tuple ->
                            Debug.WriteLine("Found closing delimiter '{0}'", text)
                            let delimiter = closeDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, 1 -> 
                                dic.Remove(delimiter) |> ignore
                            | true, c -> 
                                dic.[delimiter] <- c - 1
                            | _ -> 
                                Debug.WriteLine("It's a dangling closing delimiter")
                                result := None
                        | "[", List 
                        | "[|", Array 
                        | "{", SequenceOrRecord 
                        | "(", Tuple ->
                            Debug.WriteLine("Found opening delimiter '{0}'", text)
                            let delimiter = openDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, c -> 
                                dic.[delimiter] <- c + 1
                            | _ -> 
                                Debug.WriteLine("Record opening delimiter '{0}'", text)
                                dic.Add(delimiter, 1)
                                result := Some (i + 1, t.LeftColumn)
                        | _ -> ()
                | _ -> ()

            // We find the last opening delimiter
            tryFindStartDelimiter blockType dic !result (i + 1) lines
            
    match tryFindEndDelimiter (Dictionary()) (p.Line - 1) lines with
    | None -> 
        raise <| FormatException("""Found no pair of delimiters (e.g. "[ ]", "[| |]", "{ }" or "( )") around the cursor.""")
    | Some (endLine, endCol, blockType) ->
        match tryFindStartDelimiter blockType (Dictionary()) None 0 lines with
        | None ->
            raise <| FormatException("""Found no pair of delimiters (e.g. "[ ]", "[| |]", "{ }" or "( )") around the cursor.""")
        | Some (startLine, startCol) ->
            formatSelectionFromString isFsiFile (makeRange startLine startCol endLine endCol) s config
