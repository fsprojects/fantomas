module Fantomas.Core.Tests.ChainTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``appUnit dot identifier`` () =
    formatSourceString
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
let ``appUnit DotSet identifier`` () =
    formatSourceString
        """
X().Y <- true
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
X().Y <- true
"""

[<Test>]
let ``appParen dot identifier`` () =
    formatSourceString
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
        """
A.B().C<'d>()
"""
        { config with MaxLineLength = 10 }
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
        """
A.B().C<'d>
"""
        { config with MaxLineLength = 10 }
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
        """
A.B.C(D).E().[0]
"""
        { config with MaxLineLength = 10 }
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
        """
A.B.C(D).E.[0]
"""
        { config with MaxLineLength = 10 }
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
        """
Map
    .empty<_, obj>
    .Add("headerAction", modifyHeader.Action.ArmValue)
"""
        { config with MaxLineLength = 55 }
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
        """
type Duck() =
    member this.Duck  = Duck ()
    member this.Goose() = Duck()
    
let d = Duck()

d.Duck.Duck.Duck.Goose().Duck.Goose().Duck.Duck.Goose().Duck.Duck.Duck.Goose().Duck.Duck.Duck.Duck.Goose()
"""
        { config with MaxLineLength = 45 }
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
        """
Universe.Galaxy.SolarSystem.Planet.[3].Countries.[9].People.Count
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
Universe.Galaxy.SolarSystem.Planet.[3].Countries
    .[9].People.Count
"""

[<Test>]
let ``even longer chain with only simple links`` () =
    formatSourceString
        """
Fooooooooooo.Baaaaaaaaaaaaaaaaar.Foooooooooooooooooo.Baaaaaaaar.Basssss.Baazzzzzzzzzzzzzzzzzz.[0].Meeeeeeeeeeeeeeeeeh
    .Moooooooooooooooo.Booooooooooooooooooooh.Yooooooooooooooou.Meeeeeeh.Meh2
"""
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        """
Fooooooooooo.Baaaaaaaaaaaaaaaaar
    .Foooooooooooooooooo.Baaaaaaaar.Basssss
    .Baazzzzzzzzzzzzzzzzzz.[0].Meeeeeeeeeeeeeeeeeh
    .Moooooooooooooooo.Booooooooooooooooooooh
    .Yooooooooooooooou.Meeeeeeh.Meh2
"""

[<Test>]
let ``dot get with index without dot expression , 2761`` () =
    formatSourceString
        """
x().y[0].zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
"""
        config
    |> prepend newline
    |> should
        equal
        """
x().y[0].zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz
"""

[<Test>]
let ``don't add extra space in index without dot expression, 2760`` () =
    formatSourceString
        """
x().y[0].z // spaces inserted around index
x().y.[0].z // no spaces inserted
x().y[0] // no spaces inserted
x.y[0].z // no spaces inserted
"""
        config
    |> prepend newline
    |> should
        equal
        """
x().y[0].z // spaces inserted around index
x().y.[0].z // no spaces inserted
x().y[0] // no spaces inserted
x.y[0].z // no spaces inserted
"""

[<Test>]
let ``multiple idents in dotget with index without dot`` () =
    formatSourceString
        """
v().w.x.y.z['a'].b
"""
        config
    |> prepend newline
    |> should
        equal
        """
v().w.x.y.z['a'].b
"""

[<Test>]
let ``multiple line type expression with dotget, 3132`` () =
    formatSourceString
        """
Animal<
    Identifier
 >
    .Dog(
        "Spot"
    )
"""
        { config with MaxLineLength = 10 }
    |> prepend newline
    |> should
        equal
        """
Animal<
    Identifier
 >
    .Dog(
        "Spot"
    )
"""
