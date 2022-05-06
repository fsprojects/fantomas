module Fantomas.Core.Tests.SingleExprTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``comment after do keyword, 1875`` () =
    formatSourceString
        false
        """
do  // Comment DO
        let x = 1
        f (x) 
"""
        config
    |> prepend newline
    |> should
        equal
        """
do // Comment DO
    let x = 1
    f (x)
"""

[<Test>]
let ``comment after do bang keyword`` () =
    formatSourceString
        false
        """
async {
    do!  // Comment DO BANG
            let x = 1
            f (x) }
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    do! // Comment DO BANG
        let x = 1
        f (x)
}
"""

[<Test>]
let ``comment after return bang keyword`` () =
    formatSourceString
        false
        """
async {
    return!  // Comment RETURN BANG
        foobar()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    return! // Comment RETURN BANG
        foobar ()
}
"""

[<Test>]
let ``comment after yield bang keyword`` () =
    formatSourceString
        false
        """
seq {
    yield!  // Comment YIELD BANG
        foobar()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
seq {
    yield! // Comment YIELD BANG
        foobar ()
}
"""

[<Test>]
let ``comment after return keyword`` () =
    formatSourceString
        false
        """
async {
    return  // Comment RETURN
        foobar()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    return // Comment RETURN
        foobar ()
}
"""

[<Test>]
let ``comment after yield keyword`` () =
    formatSourceString
        false
        """
seq {
    yield  // Comment YIELD
        foobar()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
seq {
    yield // Comment YIELD
        foobar ()
}
"""

[<Test>]
let ``comment after assert keyword`` () =
    formatSourceString
        false
        """
assert // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
assert // comment
    foobar
"""

[<Test>]
let ``comment after downcast keyword`` () =
    formatSourceString
        false
        """
downcast // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
downcast // comment
    foobar
"""

[<Test>]
let ``comment after upcast keyword`` () =
    formatSourceString
        false
        """
upcast // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
upcast // comment
    foobar
"""

[<Test>]
let ``comment after address of token`` () =
    formatSourceString
        false
        """
& // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
& // comment
    foobar
"""

[<Test>]
let ``comment after address of tokens`` () =
    formatSourceString
        false
        """
&& // comment
    foobar
"""
        config
    |> prepend newline
    |> should
        equal
        """
&& // comment
    foobar
"""

[<Test>]
let ``parse `fixed` in SingleExpr, 2112`` () =
    formatSourceString false """let x = fixed expr""" config
    |> prepend newline
    |> should
        equal
        """
let x = fixed expr
"""

[<Test>]
let ``handle comment after `fixed` in SingleExpr, 2112`` () =
    formatSourceString
        false
        """
let a = fixed // comment
                 b
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    fixed // comment
        b
"""
