module Fantomas.Tests.TriviaTests

open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.TriviaTypes
    
let private toTrivia source =
    let ast = parse false source
    let tokens = TokenParser.tokenize [] source
    Trivia.collectTrivia tokens ast

[<Test>]
let ``Line comment that starts at the beginning of a line added to trivia`` () =
    let source = """// meh
let a = 9
"""

    let triviaNodes = toTrivia source
    
    match triviaNodes with
    | [{ CommentsBefore = [LineCommentOnSingleLine(lineComment)];  }] ->
        lineComment == "// meh"
    | _ ->
        failwith "Expected line comment"
//        
//// TODO: discuss whether the Trivia LineComment should include the range?
//        
[<Test>]
let ``Line comment that is alone on the single, preceded by whitespaces`` () =
    let source = """    // foo
let a = 'c'
"""

    let triviaNodes = toTrivia source
    
    match triviaNodes with
    | [{ CommentsBefore = [LineCommentOnSingleLine(lineComment)];  }] ->
        lineComment == "// foo"
    | _ ->
        failwith "Expected line comment"
        
[<Test>]
let ``Line comment on same line, is after last AST item`` () =
    let source = "let foo = 7 // should be 8"
    let triviaNodes = toTrivia source

    match triviaNodes with
    | [{CommentsAfter = [LineCommentAfterSourceCode(lineComment)]}] ->
        lineComment == "// should be 8"
    | _ ->
        fail()

[<Test>]
let ``Newline pick up before let binding`` () =
    let source = """let a = 7

let b = 9
"""
    let triviaNodes = toTrivia source

    match triviaNodes with
    | [{NewlinesBefore = newLinesBefore}] ->
        newLinesBefore == 1
    | _ ->
        fail()
        
[<Test>]
let ``Multiple comments should be linked to same trivia node`` () =
    let source = """// foo
// bar
let a = 7
"""

    let triviaNodes = toTrivia source

    match triviaNodes with
    | [{CommentsBefore = [LineCommentOnSingleLine(fooComment);LineCommentOnSingleLine(barComment)]}] ->
        fooComment == "// foo"
        barComment == "// bar"
    | _ ->
        fail()


[<Test>]
let ``Comments inside record`` () =
    let source = """let a = 
    { // foo
    B = 7 }"""
    
    let triviaNodes = toTrivia source

    match triviaNodes with
    | [{ Type = TriviaNodeType.Token(t); CommentsAfter = [LineCommentAfterSourceCode("// foo")] }] ->
        t.Content == "{"
    | _ ->
        fail()