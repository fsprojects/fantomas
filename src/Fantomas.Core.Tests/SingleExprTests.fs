module Fantomas.Core.Tests.SingleExprTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``comment after do keyword, 1875`` () =
    formatSourceString
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
    formatSourceString """let x = fixed expr""" config
    |> prepend newline
    |> should
        equal
        """
let x = fixed expr
"""

[<Test>]
let ``handle comment after `fixed` in SingleExpr, 2112`` () =
    formatSourceString
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
