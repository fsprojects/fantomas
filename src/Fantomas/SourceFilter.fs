module Fantomas.SourceFilter

// This module filters comments and compiler directives based on their locations
// and try to reattach them to the source code while pretty-printing.
//
// After extensive search, the only relevant paper is http://www.cs.kent.ac.uk/projects/refactor-fp/publications/tool-support-for-rfp.pdf
// where comments are preserved after refactoring.
//
// Comments will be preserved as follows:
//   1. Any number of commented lines before a let binding (skipping attributes) will be attached to that binding.
//   2. The same heuristic is done for type declarations and member bindings.
//   3. We would like to attach comments to patterns and expressions, but it's difficult to find out their boundaries 
//      so comments are associated with identifiers.
//   4. Any commented lines in the end of files will be copied to the results.
//
// Tentative solution:
//  1. Lex the source file and obtain a token stream.
//  2. At some keyword tokens such as 'let', 'type', 'member', etc try to go backwards to find comment tokens.
//  3. If we find some attributes, skip them.
//  4. Find first comment token, go backwards until find a token of another kind (except whitespace tokens).
//  5. If found no comment token, no entry will be created.
//  6. Add blocks of comments into a map keyed by locations of keyword tokens.
// 
// Compiler directives need more thorough treatments.
// Some hindrances are (1) They may or may not have an else branch (2) They can be nested.
// Compiler directives can be looked up by line numbers.
// The problem is to associate line numbers with the AST.

open System
open System.Text
open System.Collections.Generic

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

[<RequireQualifiedAccess>]
module Array =
    let inline last (xs : _ []) = xs.[xs.Length-1]

type Token = 
    | Token of string * TokenInformation * int
    static member content (Token(s, _, _)) = s
    static member tokenInfo (Token(_, tok, _)) = tok
    static member lineNumber (Token(_, _, n)) = n

/// Return a list of tokens with line and column information
let tokenize (s : string) =
    let lines = s.Split([|'\n'|], StringSplitOptions.None)

    let fileName = "/tmp.fs"
    let sourceTok = SourceTokenizer([], fileName)

    [| let state = ref 0L
       for n, line in lines |> Seq.zip [ 1 .. lines.Length ] do
           let tokenizer = sourceTok.CreateLineTokenizer(line)
           let rec parseLine() = seq {
              match tokenizer.ScanToken(!state) with
              | Some(tok), nstate ->
                  let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
                  yield Token(str, tok, n)
                  state := nstate
                  yield! parseLine()
              | None, nstate -> state := nstate }
           yield! parseLine() |> List.ofSeq |]

/// Search an array starting from the end
let searchBackward f (xs : _ []) =
    let rec loop i =
        if i < 0 then None
        elif f xs.[i] then Some i
        else loop (i - 1)
    loop (xs.Length - 1)

/// Skip all spaces at the end of xs
let (|Spaces|) (xs : Token []) =
    let rec loop i =
        if i < 0 then i
        else
            match xs.[i] with
            | Token(_, tok, _) when tok.CharClass = TokenCharKind.WhiteSpace -> loop (i - 1)
            | _ -> i
    xs.[..loop (xs.Length - 1)]

/// Recognize an attribute and skip it
let (|Attribute|_|) (Spaces xs) =
    if xs = [||] then None
    else
        match Array.last xs with
        | Token(">]", _, _) -> 
            match xs |> searchBackward (fun (Token(s, tok, _)) -> s = "[<" && tok.CharClass = TokenCharKind.Delimiter) with
            | Some i -> Some xs.[..i - 1]
            | None -> None
        | _ -> None

/// Recognize a list of attributes and return the array fragment before that
let rec (|Attributes|_|) = function
    | Attribute(Attributes xs)
    | Attribute xs -> Some xs
    | _ -> None

/// Return a block of comments and the array fragment before the comment block
let (|CommentBlock|_|) (Spaces xs) =
    let rec loop i acc =
        if i < 0 then (acc, i)
        else
            let (Token(_, tok, _)) = xs.[i]
            if tok.CharClass = TokenCharKind.Comment || tok.CharClass = TokenCharKind.LineComment then
                loop (i - 1) (xs.[i]::acc)
            else
                (acc, i)
    match loop (xs.Length - 1) [] with
    | [], _ -> None
    | ts, i -> Some(ts, xs.[..i])

/// Retrieve comments in a list of lines, skip some whitespaces
let rec (|CommentBlocks|_|) xs =
    let rec (|CommentBlocks|_|) = function
        | CommentBlock(ts, CommentBlocks(tss, xs)) -> Some(ts::tss, xs)
        | CommentBlock(ts, xs) -> Some([ts], xs)
        | _ -> None
    match xs with
    | CommentBlocks(tss, xs) -> 
        let ts =
            tss 
            |> List.rev 
            |> Seq.concat
            |> Seq.groupBy Token.lineNumber 
            |> Seq.map (snd >> Seq.map Token.content >> String.concat "")
            |> Seq.map (fun s -> s.TrimEnd('\r'))
        Some(ts, xs)
    | _ -> None

/// Keyword and identifier tokens have attached comments
let (|SupportedToken|_|) (Token(s, tok, n)) =
    if tok.CharClass = TokenCharKind.Keyword || tok.CharClass = TokenCharKind.Identifier then Some(s, tok, n)
    else None

/// Given a list of token, attach comments to appropriate positions
let filterComments (xs : Token []) =
    let rec loop i (xs : Token []) (dic : Dictionary<_, _>)  = 
        if i <= 0 then dic
        else
            match xs.[i] with
            | SupportedToken(_, tok, n) ->
                match xs.[..i-1] with
                | Attributes(CommentBlocks(ts, xs))
                | CommentBlocks(ts, xs) ->
                    dic.Add(mkPos n tok.LeftColumn, ts)
                    loop (xs.Length - 1) xs dic
                | _ -> loop (i - 1) xs dic           
            | _ -> loop (i - 1) xs dic
    loop (xs.Length - 1) xs (Dictionary())
