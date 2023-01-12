module Fantomas.Core.Tests.ChainTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``appUnit dot identifier`` () =
    formatSourceString
        false
        """
X().Y
"""
        config
    |> prepend newline
    |> should
        equal
        """
X().Y
"""

[<Test>]
let ``appParen dot identifier`` () =
    formatSourceString
        false
        """
X(a).Y
"""
        config
    |> prepend newline
    |> should
        equal
        """
X(a).Y
"""

[<Test>]
let ``appUnit dot appUnit`` () =
    formatSourceString
        false
        """
X().Y()
"""
        config
    |> prepend newline
    |> should
        equal
        """
X().Y()
"""

[<Test>]
let ``typed appUnit dot identifier`` () =
    formatSourceString
        false
        """
X<a>().Y
X<a>().Y<b>()
"""
        config
    |> prepend newline
    |> should
        equal
        """
X<a>().Y
X<a>().Y<b>()
"""

[<Test>]
let ``appParenLambda dot identifier`` () =
    formatSourceString
        false
        """
X(fun x -> x).Y
"""
        config
    |> prepend newline
    |> should
        equal
        """
X(fun x -> x).Y
"""

[<Test>]
let ``identifier dot appUnit dot identifier`` () =
    formatSourceString
        false
        """
X.Y().Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
X.Y().Z
"""

[<Test>]
let ``identifier dot indexed expr dot identifier`` () =
    formatSourceString
        false
        """
A.[0].B
"""
        config
    |> prepend newline
    |> should
        equal
        """
A.[0].B
"""

[<Test>]
let ``identifier dot indexed expr dot appParenExpr`` () =
    formatSourceString
        false
        """
A.[0].B(1)
"""
        config
    |> prepend newline
    |> should
        equal
        """
A.[0].B(1)
"""

[<Test>]
let ``identifier dot typed appUnit dot identifier`` () =
    formatSourceString
        false
        """
X.Y<a>().Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
X.Y<a>().Z
"""

[<Test>]
let ``identifier dot typed identifier dot identifier`` () =
    formatSourceString
        false
        """
X.Y<a>.Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
X.Y<a>.Z
"""

[<Test>]
let ``appUnit dot appParen`` () =
    formatSourceString
        false
        """
A().B(fun b -> b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
A().B(fun b -> b)
"""

[<Test>]
let ``space before lambda should not occur in chain, 2685 `` () =
    formatSourceString
        false
        """
module A =
    let foo =
        Foai.SomeLongTextYikes().ConfigureBarry(fun alpha beta gamma ->
            context.AddSomething ("a string") |> ignore
        ).MoreContext(fun builder ->
            // also good stuff
            ()
        ).ABC().XYZ
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
module A =
    let foo =
        Foai
            .SomeLongTextYikes()
            .ConfigureBarry(fun alpha beta gamma -> context.AddSomething ("a string") |> ignore)
            .MoreContext(fun builder ->
                // also good stuff
                ())
            .ABC()
            .XYZ
"""

[<Test>]
let ``identifier dot appUnit dot typed appUnit `` () =
    formatSourceString
        false
        """
A.B().C<'d>()
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
A
    .B()
    .C<'d>()
"""

[<Test>]
let ``identifier dot appUnit dot typed identifier `` () =
    formatSourceString
        false
        """
A.B().C<'d>
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
A
    .B()
    .C<'d>
"""

[<Test>]
let ``identifier dot identifier dot appExpr dot appUnit dot index expr`` () =
    formatSourceString
        false
        """
A.B.C(D).E().[0]
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
A.B
    .C(D)
    .E()
    .[0]
"""

[<Test>]
let ``identifier dot identifier dot appExpr dot identifier dot index expr`` () =
    formatSourceString
        false
        """
A.B.C(D).E.[0]
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
A.B
    .C(D)
    .E.[0]
"""

[<Test>]
let ``trivia inside chain, 2686`` () =
    formatSourceString
        false
        """
builder.
    FirstThing<X>(fun lambda ->
        // aaaaaa
        ()
    )
    .SecondThing<Y>(fun next ->
        // bbbbb
        next
    )
    // ccccc
    .ThirdThing<Z>().X
"""
        { config with
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
builder
    .FirstThing<X>(fun lambda ->
        // aaaaaa
        ()
    )
    .SecondThing<Y>(fun next ->
        // bbbbb
        next
    )
    // ccccc
    .ThirdThing<Z>()
    .X
"""
