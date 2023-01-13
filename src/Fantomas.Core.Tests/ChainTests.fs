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

[<Test>]
let ``leading type app with two identifiers, 2705`` () =
    formatSourceString
        false
        """
Map
    .empty<_, obj>
    .Add("headerAction", modifyHeader.Action.ArmValue)
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
Map.empty<_, obj>
    .Add("headerAction", modifyHeader.Action.ArmValue)
"""

[<Test>]
let ``all simple links should be on the same line, 2712`` () =
    formatSourceString
        false
        """
type Duck() =
    member this.Duck  = Duck ()
    member this.Goose() = Duck()
    
let d = Duck()

d.Duck.Duck.Duck.Goose().Duck.Goose().Duck.Duck.Goose().Duck.Duck.Duck.Goose().Duck.Duck.Duck.Duck.Goose()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Duck() =
    member this.Duck = Duck()
    member this.Goose() = Duck()

let d = Duck()

d.Duck.Duck.Duck
    .Goose()
    .Duck.Goose()
    .Duck.Duck.Goose()
    .Duck.Duck.Duck.Goose()
    .Duck.Duck.Duck.Duck.Goose()
"""

[<Test>]
let ``very long chain with a some index expressions`` () =
    formatSourceString
        false
        """
Universe.Galaxy.SolarSystem.Planet.[3].Countries.[9].People.Count
"""
        { config with
            MaxDotGetExpressionWidth = 0 }
    |> prepend newline
    |> should
        equal
        """
Universe
    .Galaxy.SolarSystem.Planet.[3].Countries.[9].People.Count
"""
