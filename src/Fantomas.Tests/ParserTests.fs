module Fantomas.Tests.ParserTests

open NUnit.Framework
open FsUnit

open Fantomas.Ast
open Fantomas.Parser

[<Test>]
let ``simple declarations``() =
    "let x = 42" |> parseExps |> should equal [[Let (false,[(PVar "x", Lit (Int 42))],Lit Unit)]]
    "let x = 42\n\
     let y = 24" |>  parseExps |> should equal [[Let (false,[(PVar "x", Lit (Int 42))],Lit Unit)];
                                                [Let (false,[(PVar "y", Lit (Int 24))],Lit Unit)]]

