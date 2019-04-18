module Fantomas.Tests.TriviaTests

open System.Collections.Generic
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.Trivia

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
            Assert.Pass()
        | _ ->
            Assert.Fail()
    | _ ->
        Assert.Fail()
    
    match astNode, triviaNode with
    | _ , [{ CommentsBefore = [LineComment(lineComment)];  }] ->
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
    | :? FSharp.Compiler.Ast.SynModuleDecl as smd ->
        match smd with
        | FSharp.Compiler.Ast.SynModuleDecl.Let(_,_,_) ->
            Assert.Pass()
        | _ ->
            Assert.Fail()
    | _ ->
        Assert.Fail()
    
    match astNode, triviaNode with
    | _ , [{ CommentsBefore = [LineComment(lineComment)];  }] ->
        lineComment == "// foo"
    | _ ->
        failwith "Expected line comment"