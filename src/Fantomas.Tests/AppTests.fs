module Fantomas.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the |> is merged to the last line 
[<Test>]
let ``no nln before lambda #503``() =
    formatSourceString false """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    """ { config with PageWidth = 80 }
    |> prepend newline
    |> should equal """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
"""

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545``() =
    formatSourceString false @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               s
                                     s"
        config
    |> prepend newline
    |> should equal @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""    (llloooooooooooooooooooooooooo s) s s s
"

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545 (long expression and short line settings)``() =
    formatSourceString false @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               (llloooooooooooooooooooooooooo s)
                                     (llloooooooooooooooooooooooooo s)"
        { config  with PageWidth = 50 } 
    |> prepend newline
    |> should equal @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""    (llloooooooooooooooooooooooooo s) s
            (llloooooooooooooooooooooooooo s)
            (llloooooooooooooooooooooooooo s)
"
