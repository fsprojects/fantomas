module Fantomas.Tests.TokenParserTests

open NUnit.Framework
open FsUnit
open Fantomas.TokenParser
open Fantomas.Tests.TestHelper

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
    |> List.length
    |> should equal 7
    
[<Test>]
let ``tokenize should return correct sequence of tokens`` () =
    let source = "let a = 7" // LET WHITESPACE IDENT WHITESPACE EQUALS WHITESPACE INT32
    let tokens = tokenize [] source |> List.map (fun t -> t.TokenInfo.TokenName) 
    tokens.[0] == "LET"
    tokens.[1] == "WHITESPACE"
    tokens.[2] == "IDENT"
    tokens.[3] == "WHITESPACE"
    tokens.[4] == "EQUALS"
    tokens.[5] == "WHITESPACE"
    tokens.[6] == "INT32"
    
[<Test>]
let ``simple line comment should be found in tokens`` () =
    let source = "let a = 7 // some comment"
    let tokens = tokenize [] source
    let additionalInfo = getAdditionalInfoFromTokens tokens []
    
    match List.tryLast additionalInfo with
    | Some(Comment(LineComment(lineComment)), range) ->
        lineComment == "// some comment"
        range.Start.Line == range.End.Line
        
    | _ ->
        failwith "expected comment"
    
[<Test>]
let ``keyword should be found in tokens`` () =
    let source = "let a = 42"
    let tokens = tokenize [] source
    let additionalInfo = getAdditionalInfoFromTokens tokens []
    
    match List.tryHead additionalInfo with
    | Some(Keyword(keyword), range) ->
        keyword == "let"
        range.Start.Column == 0
        range.Start.Line == 0
        range.End.Column == 2
        range.End.Line == 0
    | _ ->
        failwith "expected keyword"
        
[<Test>]
let ``Xml comment should be found in tokens`` () =
    let source = """/// Regex alone won't cut it, good enough for now
let getDefines sourceCode =
    Regex.Matches(sourceCode, "#if\\s(\\S+)")
    |> Seq.cast<Match>
    |> Seq.map (fun mtc -> mtc.Value.Substring(4))
    |> Seq.toArray
"""
    let tokens = tokenize [] source
    let additionalInfo = getAdditionalInfoFromTokens tokens []
    
    match List.tryHead additionalInfo with
    | Some(Comment(XmlComment(xmlComment)), _) ->
        xmlComment == "/// Regex alone won't cut it, good enough for now"
    | _ ->
        failwith "expected xml comment"