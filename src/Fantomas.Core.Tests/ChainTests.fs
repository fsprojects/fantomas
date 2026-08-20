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
Map.empty<_, obj>.Add(
    "headerAction",
    modifyHeader.Action.ArmValue
)
"""

[<Test>]
let ``dotlambda chain with simple segments`` () =
    formatSourceString
        """
_.Name.Length
"""
        { config with MaxLineLength = 12 }
    |> prepend newline
    |> should
        equal
        """
_.Name
    .Length
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
Universe.Galaxy.SolarSystem.Planet
    .[3].Countries.[9].People.Count
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

// ── Tight receivers ─────────────────────────────────────────────────────────
//
// `mkAtomicExpr` in the ASTTransformer marks a chain that has to stay one indivisible unit,
// because a prefix operator, an index or a `?member` binds directly to it. The tests below
// cover the prefix-operator, index and `?`-chain call sites. They all run with
// `SpaceBeforeUppercaseInvocation = true`,
// because that is the setting that would otherwise introduce the space: compare with
// `obj.Bar()` on its own, which correctly becomes `obj.Bar ()` under the same config.

[<Test>]
let ``tight receiver control case, a plain terminal call does take the space`` () =
    formatSourceString
        """
obj.Bar()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
obj.Bar ()
"""

[<Test>]
let ``tight receiver, leading expression of a dynamic chain`` () =
    formatSourceString
        """
obj?A()?B()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
obj?A()?B()
"""

[<Test>]
let ``tight receiver, prefix operator applied to a unit call`` () =
    formatSourceString
        """
-obj.Bar()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
-obj.Bar()
"""

[<Test>]
let ``tight receiver, prefix operator applied to a paren call`` () =
    formatSourceString
        """
-obj.Bar(a)
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
-obj.Bar(a)
"""

[<Test>]
let ``tight receiver, identifier of a new-style index`` () =
    formatSourceString
        """
a.Foo()[0]
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
a.Foo()[0]
"""

// ── Intermediate calls stay welded to their opening paren ───────────────────
//
// An intermediate call may never be separated from its `(`: `a.Foo (x).Bar()` parses as
// `a.Foo ((x).Bar())`. A conditional directive attached to the argument pushes that
// argument onto its own lines, and the break has to land AFTER the `(`, not before it.

[<Test>]
let ``directive inside an intermediate call argument keeps the opening paren tight`` () =
    formatSourceString
        """
let x =
    builder.Configure(
#if DEBUG
        debugOptions
#else
        releaseOptions
#endif
    ).Build().Result
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    builder
        .Configure(
#if DEBUG
            debugOptions
#else
            releaseOptions
#endif
        )
        .Build()
        .Result
"""

[<Test>]
let ``match lambda as an intermediate call argument keeps the function keyword attached`` () =
    // Identical in shape to the terminal case below: where the call sits in the chain has no
    // say over a lambda argument, for `function` just as for `fun`.
    formatSourceString
        """
let x =
    builder.Configure(function
        | Some v -> handleSome v
        | None -> handleNone ()).Build().Result
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    builder
        .Configure(function
            | Some v -> handleSome v
            | None -> handleNone ())
        .Build()
        .Result
"""

[<Test>]
let ``match lambda as a terminal call argument keeps the function keyword attached`` () =
    formatSourceString
        """
let x =
    builder.Build().Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    builder
        .Build()
        .Configure(function
            | Some v -> handleSome v
            | None -> handleNone ())
"""

// ── Casing of the terminal is decided by the LAST segment ───────────────────
//
// `SpaceBeforeUppercaseInvocation` looks at the name the terminal call is made on,
// which is always the final segment. Intermediate calls earlier in the chain have no
// say — and stay tight regardless of their own casing.

[<Test>]
let ``uppercase terminal after an uppercase intermediate call takes the space`` () =
    formatSourceString
        """
a.Foo(x).Bar(y)
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
a.Foo(x).Bar (y)
"""

[<Test>]
let ``lowercase terminal after an uppercase intermediate call does not take the space`` () =
    formatSourceString
        """
a.Foo(x).bar(y)
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeLowercaseInvocation = false }
    |> prepend newline
    |> should
        equal
        """
a.Foo(x).bar(y)
"""

// ── A match lambda as the terminal call's argument ──────────────────────────
//
// Only `MultiLineLambdaClosingNewline` or a break the user already made after the `(`
// moves `function` onto its own line. Where the call sits in the chain has no say.

[<Test>]
let ``match lambda as a terminal call argument breaks when closing newline is set`` () =
    formatSourceString
        """
let x =
    builder.Build().Configure(function
        | Some v -> handleSome v
        | None -> handleNone ())
"""
        { config with
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
let x =
    builder
        .Build()
        .Configure(
            function
            | Some v -> handleSome v
            | None -> handleNone ()
        )
"""

[<Test>]
let ``match lambda as a terminal call argument with the function keyword written below the paren`` () =
    formatSourceString
        """
let x =
    builder.Build().Configure(
        function
        | Some v -> handleSome v
        | None -> handleNone ())
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    builder
        .Build()
        .Configure(function
            | Some v -> handleSome v
            | None -> handleNone ())
"""

[<Test>]
let ``a comment before the argument of a terminal call keeps the parenthesis with the method name`` () =
    formatSourceString
        """
let host =
    builder.UseUrls(
        // the public endpoint
        url
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let host =
    builder.UseUrls(
        // the public endpoint
        url
    )
"""

[<Test>]
let ``a comment before the argument of a terminal call written on one line`` () =
    formatSourceString
        """
let host = builder.UseUrls(
    // the public endpoint
    url)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let host =
    builder.UseUrls(
        // the public endpoint
        url
    )
"""

[<Test>]
let ``a comment before the argument of an intermediate call keeps the parenthesis welded`` () =
    formatSourceString
        """
let host =
    builder
        .UseUrls(
            // the public endpoint
            url
        )
        .Build()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let host =
    builder
        .UseUrls(
            // the public endpoint
            url
        )
        .Build()
"""

[<Test>]
let ``a comment between the method name and the parenthesis takes the call down with it`` () =
    formatSourceString
        """
let host =
    builder.UseUrls
        // pick the endpoint
        (url)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let host =
    builder.UseUrls
        // pick the endpoint
        (url)
"""

[<Test>]
let ``a comment between the method name and the parenthesis leaves the argument rules alone`` () =
    formatSourceString
        """
let host =
    builder.UseUrls
        // pick the endpoint
        (theConfigurationValueForThePublicEndpoint, theFallbackEndpointValue)
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let host =
    builder.UseUrls
        // pick the endpoint
        (
            theConfigurationValueForThePublicEndpoint,
            theFallbackEndpointValue
        )
"""

// A comment written after the receiver ends its line before the chain has decided anything.
// The steps behind it have to open an indented line, or they land level with the receiver and
// the result no longer parses. Where the comment attaches depends on how it was written, so
// the three spellings below are the same chain and have to reach the same output.

[<Test>]
let ``a comment on its own line after the receiver indents the steps behind it`` () =
    formatSourceString
        """
let a =
    config
    // note
        .Settings.GetValue(key)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    config
        // note
        .Settings.GetValue(key)
"""

[<Test>]
let ``the column the comment after a receiver was written at makes no difference`` () =
    formatSourceString
        """
let a =
    config
        // note
        .Settings.GetValue(key)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    config
        // note
        .Settings.GetValue(key)
"""

[<Test>]
let ``a trailing comment after the receiver indents the steps behind it`` () =
    formatSourceString
        """
let a =
    config // note
        .Settings.GetValue(key)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    config // note
        .Settings.GetValue(key)
"""
