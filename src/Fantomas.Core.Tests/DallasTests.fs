module Fantomas.Core.Tests.DallasTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``proof of concept`` () =
    formatSourceString
        false
        """
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 1
"""

[<Test>]
let ``named module with let binding`` () =
    formatSourceString
        false
        """
module A.B
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A.B

let a = 1
"""

[<Test>]
let ``basic comment above let binding`` () =
    formatSourceString
        false
        """
let a =  0

// foobar
let b =  1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 0

// foobar
let b = 1
"""

[<Test>]
let ``single open`` () =
    formatSourceString
        false
        """
open  Foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Foo
"""

[<Test>]
let ``two opens`` () =
    formatSourceString
        false
        """
open  Foo
open  Bar

let a =  0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Foo
open Bar

let a = 0
"""

[<Test>]
let ``type alias`` () =
    formatSourceString
        false
        """
type A =   int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = int
"""

[<Test>]
let ``function with parameters`` () =
    formatSourceString
        false
        """
let x y z  = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y z = 0
"""
