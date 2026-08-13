module Fantomas.Core.Tests.DynamicOperatorTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``keep () when dynamic operator is used`` () =
    formatSourceString "let memoEquals x = x?(k + 1)" config
    |> should
        equal
        """let memoEquals x = x?(k + 1)
"""

[<Test>]
let ``remove () when dynamic operator is string`` () =
    formatSourceString "let memoEquals x = x?k" config
    |> should
        equal
        """let memoEquals x = x?k
"""

[<Test>]
let ``keep () when dynamic operator inside boolean expr, #476`` () =
    formatSourceString
        """let fieldColor (fieldNameX: string) =
    if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
        IsDanger
    else
        NoColor
    |> Input.Color
"""
        { config with
            MaxIfThenElseShortWidth = 5 }
    |> prepend newline
    |> should
        equal
        """
let fieldColor (fieldNameX: string) =
    if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
        IsDanger
    else
        NoColor
    |> Input.Color
"""

[<Test>]
let ``keep () when dynamic operator inside boolean expr, 2 spaces indent`` () =
    formatSourceString
        """let fieldColor (fieldNameX: string) =
    if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
        IsDanger
    else
        NoColor
    |> Input.Color
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let fieldColor (fieldNameX: string) =
  (if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
     IsDanger
   else
     NoColor)
  |> Input.Color
"""

[<Test>]
let ``preserve back ticks from checked keyword, 937`` () =
    formatSourceString "let toggle = unbox<bool> (e.target?``checked``)" config
    |> prepend newline
    |> should
        equal
        """
let toggle = unbox<bool> (e.target?``checked``)
"""

[<Test>]
let ``case determination issue with ExprAppSingleParenArgNode, 3088`` () =
    formatSourceString
        """
let doc = x?a("")?b(t)?b(t)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let doc = x?a("")?b(t)?b(t)
"""

[<Test>]
let ``no space before paren args in dynamic operator chain, 3159`` () =
    formatSourceString
        """
x?a("")?b(t)
"""
        config
    |> prepend newline
    |> should
        equal
        """
x?a("")?b(t)
"""

[<Test>]
let ``case determination issue with ExprAppSingleParenArgNode uppercase with config lower, 3088`` () =
    // Space before paren args of a `?` result is never added, regardless of SpaceBefore(Upper|Lower)caseInvocation.
    // Adding a space changes the AST when followed by another `?`, e.g. `X?a ("arg")?B`. See #3159.
    formatSourceString
        """
let doc1 = x?a("arg")?B("barg")?c("carg")
let doc2 = X?a("arg")?B("barg")?c("carg")
"""
        { config with
            SpaceBeforeLowercaseInvocation = false
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let doc1 = x?a("arg")?B("barg")?c("carg")
let doc2 = X?a("arg")?B("barg")?c("carg")
"""

[<Test>]
let ``case determination issue with ExprParenNode uppercase with config lower, 2998`` () =
    formatSourceString
        """
let statusBarHeight = (window?getComputedStyle document.documentElement)?getPropertyValue "--statusBarHeight"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let statusBarHeight =
    (window?getComputedStyle document.documentElement)?getPropertyValue "--statusBarHeight"
"""

[<Test>]
let ``dynamic operator on result of long ident paren arg, 3135`` () =
    formatSourceString
        """
Jest.expect(json)?oMatchSnapshot ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Jest.expect(json)?oMatchSnapshot ()
"""

// A lambda or match-lambda argument is NOT fused into the `?` chain: unlike a plain
// argument (`obj?y?z (a)`, where the argument ends up inside the chain item), it stays a
// normal application so the argument can use the ordinary multiline lambda layout.

[<Test>]
let ``dynamic operator with a lambda argument`` () =
    formatSourceString
        """
let a = x?y (fun a -> a)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = x?y (fun a -> a)
"""

[<Test>]
let ``dynamic operator with a match lambda argument`` () =
    formatSourceString
        """
let b = x?y (function
             | Some v -> v
             | None -> 0)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let b =
    x?y (function
        | Some v -> v
        | None -> 0)
"""

[<Test>]
let ``dynamic chain with a lambda argument`` () =
    formatSourceString
        """
let c = obj?y?z (fun a -> a)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = obj?y?z (fun a -> a)
"""
