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

[<Test>]
let ``count function signature`` () =
    let ast =
        """
let readModel (updateState : 'State -> EventEnvelope<'Event> list -> 'State) (initState : 'State) =
    ()
"""
        |> parse false
        |> Array.head
        |> fst

    let result =
        match ast with
        | ImplFile (ParsedImplFileInput (_,
                                         [ ModuleOrNamespace (_,
                                                              _,
                                                              _,
                                                              _,
                                                              [ Let (LetBinding (_,
                                                                                 _,
                                                                                 _,
                                                                                 _,
                                                                                 _,
                                                                                 PatLongIdent (_, s, ps, _),
                                                                                 _,
                                                                                 _)) ],
                                                              _,
                                                              _) ])) ->
            CountAstNode.FunctionSignature(s, ps, None)
            |> isASTLongerThan 65
        | _ -> failwith "expected different AST"

    if result
    then pass ()
    else failwithf "expected to be over the threshold but was %A" result
