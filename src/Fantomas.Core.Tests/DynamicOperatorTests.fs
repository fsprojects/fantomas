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
    // Adding a space can generate invalid code, e.g. `x?a("arg")?b (t)`. See #3159.
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
Jest.expect(json)?oMatchSnapshot()
"""
