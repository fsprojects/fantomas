module Fantomas.Core.Tests.DotLambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``function call`` () =
    formatSourceString
        """
let x = "a" |> _.ToString()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = "a" |> _.ToString()
"""

[<Test>]
let ``property call`` () =
    formatSourceString
        """
let x = "a" |> _.Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = "a" |> _.Length
"""

[<Test>]
let ``property of method invocation`` () =
    formatSourceString
        """
let c = _.ToString().Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = _.ToString().Length
"""

[<Test>]
let ``property of function invocation`` () =
    formatSourceString
        """
let c = _.foo().Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = _.foo().Length
"""

[<Test>]
let ``idempotency problem when _.Property shorthand, 3050`` () =
    formatSourceString
        """
"ABC" |> _.ToLower()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
"ABC" |> _.ToLower()
"""

[<Test>]
let ``idempotency problem when _.property shorthand lowercase, 3050`` () =
    formatSourceString
        """
"ABC" |> _.toLower()
"""
        { config with
            SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
"ABC" |> _.toLower()
"""

[<Test>]
let ``idempotency problem when _.property shorthand quoted, 3050`` () =
    formatSourceString
        """
"ABC" |> _.``to Lower``()
"""
        { config with
            SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
"ABC" |> _.``to Lower``()
"""

[<Test>]
let ``idempotency problem when _.Property shorthand with app arg, 3050`` () =
    formatSourceString
        """
let Meh () = 1

type Bar() =
    member this.Foo(v:int):int = v + 1

let b = Bar()
b |> _.Foo(Meh ())
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let Meh () = 1

type Bar() =
    member this.Foo(v: int) : int = v + 1

let b = Bar ()
b |> _.Foo(Meh ())
"""
