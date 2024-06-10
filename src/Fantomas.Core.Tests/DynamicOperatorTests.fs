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
let doc = x?a ("")?b (t)?b (t)
"""

[<Test>]
let ``case determination issue with ExprAppSingleParenArgNode uppercase with config lower, 3088`` () =
    // We want to disobey SpaceBefore(Upper|Lower)caseInvocation inside of the ? chain because mixing it up can generate invalid code like x?a("arg")?B ("barg")?c("carg")
    // The space config that is used (Upper or Lower) depends on the case of the dynamic object, here x
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
let doc2 = X?a ("arg")?B ("barg")?c ("carg")
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
