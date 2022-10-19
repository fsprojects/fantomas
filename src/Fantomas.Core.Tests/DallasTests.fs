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

[<Test>]
let ``ident expr with backticks`` () =
    formatSourceString
        false
        """
let x =  ``y``
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = ``y``
"""

[<Test>]
let ``horsing around`` () =
    formatSourceString
        false
        """
null
<@@  x @@>
<@  y @>
x :>  int
new T()
new T(t1)
let null =  0
let _ =  0
let () = 0
let 4 = 1
let a (b) = c
let a,b = 0
let a ?b = c
let a ([<Foo>] b) = 0
let (a | b) = 0
let (a & b & c) = 0
let x (a: int) = 0
let (x as y) = 0
let (x :: y) = 0
let X(y = 0) = 0
let (X(y,z)) = 0
let (struct (a,b)) = 0
let [ a; b ] = 0
let <@ a @> = 0
let (:? a) = 0
let { A = a; B.B = b } = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
null
<@@ x @@>
<@ y @>
x :> int
new T()
new T(t1)
let null = 0
let _ = 0
let () = 0
let 4 = 1
let a (b) = c
let a, b = 0
let a ?b = c
let a ([<Foo>] b) = 0
let (a | b) = 0
let (a & b & c) = 0
let x (a: int) = 0
let (x as y) = 0
let (x :: y) = 0
let X(y = 0) = 0
let (X(y, z)) = 0
let (struct (a, b)) = 0
let [ a; b ] = 0
let <@ a @> = 0
let (:? a) = 0
let { A = a; B.B = b } = 0
"""

[<Test>]
let ``some types`` () =
    formatSourceString
        false
        """
let a (b: (int)) = 0
let x (y : int[,,]) = 0
type t = int -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a (b: (int)) = 0
let x (y: int[,,]) = 0
type t = int -> int
"""
