module Fantomas.Tests.TokenParserTests

open NUnit.Framework
open FsUnit
open Fantomas.TokenParser
open Fantomas.TriviaTypes
open Fantomas.Tests.TestHelper

let private isNewline item =
    match item with
    | Newline -> true
    | _ -> false

[<Test>]
let ``Simple compiler directive should be found`` () =
    let source = """
#if DEBUG
setupServer false
#else
setupServer true
#endif
"""

    getDefines source
    |> Array.length
    |> should equal 1
    
[<Test>]
let ``Simple compiler directive should be DEBUG`` () =
    let source = """
#if DEBUG
setupServer false
#else
setupServer true
#endif
"""

    getDefines source
    |> Array.head
    |> should equal "DEBUG"
    
[<Test>]
let ``tokenize should return correct amount`` () =
    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32
    tokenize [] source
    |> fst
    |> List.length
    |> should equal 7
    
[<Test>]
let ``tokenize should return correct sequence of tokens`` () =
    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32
    let tokens = tokenize [] source |> fst |> List.map (fun t -> t.TokenInfo.TokenName)
    tokens.[0] == "LET"
    tokens.[1] == "WHITESPACE"
    tokens.[2] == "IDENT"
    tokens.[3] == "WHITESPACE"
    tokens.[4] == "EQUALS"
    tokens.[5] == "WHITESPACE"
    tokens.[6] == "INT32"

[<Test>]
let ``tokenize should work with multiple lines`` () =
    let source = """let a = 8
let b = 9"""
    let (tokens,_) = tokenize [] source
    let tokensLength = List.length tokens
    tokensLength == 14
    
    let aTokens = List.filter (fun t -> t.LineNumber = 1) tokens
    let aTok = List.item 2 aTokens
    aTok.Content == "a"
    
    let bTokens = List.filter (fun t -> t.LineNumber = 2) tokens
    let bTok = List.item 2 bTokens
    bTok.Content == "b"

[<Test>]
let ``simple line comment should be found in tokens`` () =
    let source = "let a = 7 // some comment"
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match List.tryLast triviaNodes with
    | Some({ Item = Comment(LineCommentAfterSourceCode(lineComment)) ; Range = range}) ->
        lineComment == "// some comment"
        range.StartLine == range.EndLine
        
    | _ ->
        failwith "expected comment"

[<Test>]
let ``Single line block comment should be found in tokens`` () =
    let source = "let foo (* not fonz *) = \"bar\""
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match List.tryLast triviaNodes with
    | Some({ Item = Comment(BlockComment(blockComment)) }) ->
        blockComment == "(* not fonz *)"
    | _ ->
        failwith "expected block comment"
        
[<Test>]
let ``Multi line block comment should be found in tokens`` () =
    let source = """let bar =
(* multi
   line
   comment *)
    7"""
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match List.tryLast triviaNodes with
    | Some({ Item = Comment(BlockComment(blockComment)); Range = range }) ->
        blockComment == """(* multi
   line
   comment *)"""
        range.StartLine == 2
        range.EndLine == 4
    | _ ->
        failwith "expected block comment"
        
[<Test>]
let ``Multiple line comment should be found in tokens`` () =
    let source = """// meh
// foo
let a = 9
"""
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match triviaNodes with
    | ({ Item = Comment(LineCommentOnSingleLine(l1)) })::({ Item = Comment(LineCommentOnSingleLine(l2)) })::_ ->
        l1 == "// meh"
        l2 == "// foo"
    | _ ->
        failwith "Expected two line comments"
        
[<Test>]
let ``newline should be found in tokens`` () =
    let source = """printfn "foo"

printfn "bar" """
    
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match triviaNodes with
    | [{ Item = item; Range = range }] when (isNewline item) ->
        range.StartLine == 2
        range.EndLine == 2
    | _ ->
        failwith "expected newline"
        
[<Test>]
let ``Only empty spaces in line are also consider as Newline`` () =
    let source = """printfn "foo"
    
printfn "bar" """ // difference is the 4 spaces on line 188

    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match triviaNodes with
    | [{ Item = item; Range = range }] when (isNewline item) ->
        range.StartLine == 2
        range.EndLine == 2
    | _ ->
        failwith "expected newline"
        
[<Test>]
let ``Comment after left brace of record`` () =
    let source = """let a = 
    { // foo
    B = 7 }"""
    
    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount

    match triviaNodes with
    | [ { Item = Comment(LineCommentAfterSourceCode(comment)); Range = range }] ->
        comment == "// foo"
        range.StartLine == 2
    | _ ->
        failwith "expected line comment after left brace"
        
    
//[<Test>]
//let ``keyword should be found in tokens`` () =
//    let source = "let a = 42"
//    let tokens = tokenize [] source
//    let triviaNodes = getTriviaNodesFromTokens tokens
//    
//    match List.tryHead triviaNodes with
//    | Some({ Type = Keyword(keyword); Range = range }) ->
//        keyword == "let"
//        range.StartColumn == 0
//        range.StartLine == 1
//        range.EndColumn == 2
//        range.EndLine == 1
//    | _ ->
//        failwith "expected keyword"
        
[<Test>]
let ``left brace should be found in tokens`` () =
    let source = "type R = { A: int }"
    let (tokens, _) = tokenize [] source
    let triviaNodes = getTriviaNodesFromTokens tokens
    
    match triviaNodes with
    | [{Type = Token(equals)}; {Type = Token(lbrace)} ; {Type = Token(rbrace)}] ->
        equals.Content == "="
        lbrace.Content == "{"
        rbrace.Content == "}"
    | _ ->
        fail()
        
[<Test>]
let ``leading and trailing whitespaces should be found in tokens`` () =
    let source = """
type T() =
    let x = 123
"""

    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match triviaNodes with
    | [{ Item = Newline; Range = rAbove }; {Item = Newline; Range = rBelow}] ->
        rAbove.StartLine == 1
        rBelow.EndLine == 4
    | _ ->
        fail()
        
[<Test>]
let ``if keyword should be found in tokens`` () =
    let source = """if true then ()
elif true then ()"""

    let (tokens,lineCount) = tokenize [] source
    let triviaNodes = getTriviaFromTokens tokens lineCount
    
    match triviaNodes with
    | [{Item = Keyword(``if``)}; {Item = Keyword(``elif``)}] ->
        ``if`` == "if"
        ``elif`` == "elif"
    | _ ->
        fail()
        
[<Test>]
let ``directives are found in tokens`` () =
    let source = """
#if NOT_DEFINED
#else
let x = 1
#endif
"""

    let defines = getDefines source |> List.ofArray
    let (tokens,lineCount) = tokenize defines source
    let triviaNodes =
        getTriviaFromTokens tokens lineCount
        |> List.choose (fun tv -> match tv.Item with | Directive(directive) -> Some directive | _ -> None)
        
    List.length triviaNodes == 3
    
[<Test>]
let ``Inactive code is found as well`` () =
    let source = """
let [<Literal>] private assemblyConfig() =
  #if TRACE
  let x = ""
  #else
  let x = "x"
  #endif
  x
"""

    let defines = getDefines source |> List.ofArray
    let (tokens,lineCount) = tokenize defines source
    let triviaNode =
        getTriviaFromTokens tokens lineCount
        |> List.choose (fun tv -> match tv.Item with | InActiveCode(code) -> Some code | _ -> None)
        |> List.head
        
    triviaNode == "  let x = \"x\""