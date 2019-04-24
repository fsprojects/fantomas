module Fantomas.Tests.TriviaTests

open System.Collections.Generic
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.TriviaTypes
open FSharp.Compiler.Ast

let private toTuple (kv: KeyValuePair<_,_>) =
    kv.Key, kv.Value
    
let private toTrivia source =
    let ast = parse false source
    let tokens = TokenParser.tokenize [] source
    Trivia.collectTrivia tokens ast

[<Test>]
let ``Line comment that starts at the beginning of a line added to trivia`` () =
    let source = """// meh
let a = 9
"""

    let (astNode, triviaNode) = toTrivia source |> Seq.head |> toTuple
    
    match astNode with
    | :? FSharp.Compiler.Ast.SynModuleDecl as smd ->
        match smd with
        | FSharp.Compiler.Ast.SynModuleDecl.Let(_,_,_) ->
            pass()
        | _ ->
            fail()
    | _ ->
        fail()
    
    match triviaNode with
    | [{ CommentsBefore = [LineCommentOnSingleLine(lineComment)];  }] ->
        lineComment == "// meh"
    | _ ->
        failwith "Expected line comment"
        
// TODO: discuss whether the Trivia LineComment should include the range?
        
[<Test>]
let ``Line comment that is alone on the single, preceded by whitespaces`` () =
    let source = """    // foo
let a = 'c'
"""

    let (astNode, triviaNode) = toTrivia source |> Seq.head |> toTuple
    
    match astNode with
    | :? SynModuleDecl as smd ->
        match smd with
        | SynModuleDecl.Let(_,_,_) ->
            pass()
        | _ ->
            fail()
    | _ ->
        fail()
    
    match triviaNode with
    | [{ CommentsBefore = [LineCommentOnSingleLine(lineComment)];  }] ->
        lineComment == "// foo"
    | _ ->
        failwith "Expected line comment"
        
[<Test>]
let ``Line comment on same line, is after last AST item`` () =
    let source = "let foo = 7 // should be 8"
    let (astNode, triviaNode) = toTrivia source |> Seq.head |> toTuple
    
    match astNode with
    | :? SynExpr as synExpr ->
        match synExpr with
        | SynExpr.Const(_,_) ->
           pass()
        | _ ->
            fail()
    | _ ->
        fail()
        
    match triviaNode with
    | [{CommentsAfter = [LineCommentAfterSourceCode(lineComment)]}] ->
        lineComment == "// should be 8"
    | _ ->
        fail()

[<Test>]
let ``Newline pick up before let binding`` () =
    let source = """let a = 7

let b = 9
"""
    let (astNode, triviaNode) = toTrivia source |> Seq.head |> toTuple
    
    match astNode with
    | :? SynModuleDecl as smd ->
        match smd with
        | SynModuleDecl.Let(_,_,r) ->
            r.StartLine == 3
        | _ ->
            fail()
    | _ ->
        fail()
        
    match triviaNode with
    | [{NewlinesBefore = newLinesBefore}] ->
        newLinesBefore == 1
    | _ ->
        fail()
        
[<Test>]
let ``Multiple comments should be linked to same AST node`` () =
    let source = """// foo
// bar
let a = 7
"""

    let (_, triviaNode) = toTrivia source |> Seq.head |> toTuple

    match triviaNode with
    | [{CommentsBefore = [LineCommentOnSingleLine("// foo")]};{CommentsBefore = [LineCommentOnSingleLine("// bar")]}] ->
        pass()
    | _ ->
        fail()


[<Test>]
let ``Comments inside record`` () =
    let source = """let a = 
    { // foo
    B = 7 }"""
    
    let (record, triviaNode) = toTrivia source |> Seq.head |> toTuple

    match record with
    | :? SynExpr as synRec ->
        match synRec, triviaNode with
        | SynExpr.Record _, [{ Type = LeftBrace; CommentsAfter = [LineCommentAfterLeftBrace("// foo")] }] ->
            pass()
        | _ ->
            fail()
    | _ ->
        fail()