module Fantomas.Tests.SourceCounterTests

open Fantomas
open Fantomas.SourceParser
open Fantomas.SourceCounter
open NUnit.Framework
open Fantomas.Tests.TestHelper


[<Test>]
let ``count parameters of lambda`` () =
    let parenExpr =
        """
(fun aaa bbbbbb (CCC ccc) ddddddddddd (EEEE eee) (FFF fff) ggggg ->
                failwith "Not important")
"""
        |> Input
        |> toSynExprs
        |> List.head

    let result =
        match parenExpr with
        | Paren (DesugaredLambda (cps, _)) ->
            CountAstNode.ComplexPatsList cps
            |> isASTLongerThan 40
        | _ -> failwithf "expected different ast"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result
