module Fantomas.Tests.TriviaTests

open System.Collections.Generic
open NUnit.Framework
open Fantomas
open Fantomas.Tests.TestHelper
open Fantomas.Trivia
open NUnit.Framework

let toTuple (kv: KeyValuePair<_,_>) =
    kv.Key, kv.Value

[<Test>]
let ``Line comment added to trivia`` () =
    let source = """// meh
let a = 9
"""
    let ast = parse false source
    let tokens = TokenParser.tokenize [] source
    let trivia = Trivia.collectTrivia tokens ast
    let (astNode, triviaNode) = trivia |> Seq.head |> toTuple
    
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