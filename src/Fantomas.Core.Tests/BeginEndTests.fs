module Fantomas.Core.Tests.BeginEndTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``begin end in value binding`` () =
    formatSourceString
        """
let x = begin 42 end
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = begin 42 end
"""

[<Test>]
let ``begin end in function body`` () =
    formatSourceString
        """
let f x = begin x + 1 end
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x = begin x + 1 end
"""

[<Test>]
let ``begin end in match branch`` () =
    formatSourceString
        """
let g x =
    match x with
    | 0 -> begin 1 end
    | _ -> begin 2 end
"""
        config
    |> prepend newline
    |> should
        equal
        """
let g x =
    match x with
    | 0 -> begin 1 end
    | _ -> begin 2 end
"""

[<Test>]
let ``begin end with comment inside`` () =
    formatSourceString
        """
do
    begin
        // hello
        let a = 1
        a
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
do
    begin
        // hello
        let a = 1
        a
    end
"""

[<Test>]
let ``detect begin end from SynExpr.Paren, 2368`` () =
    formatSourceString
        """
do
    begin 1 end
"""
        config
    |> prepend newline
    |> should
        equal
        """
do begin 1 end
"""

[<Test>]
let ``multiline begin end in expression`` () =
    formatSourceString
        """
do
    let a = 1
    begin
        use b = f ()
        ()
    end
    let c = 2
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
do
    let a = 1

    begin
        use b = f ()
        ()
    end

    let c = 2
    ()
"""
