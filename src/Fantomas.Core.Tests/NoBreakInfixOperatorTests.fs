module Fantomas.Core.Tests.NoBreakInfixOperatorTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

// `=`, `>`, `<`, `%` and `%%` cannot start a line at the column of the left-hand side: there the
// parser reads `=`, `>` and `<` as the `=` of a binding, and reads `%` and `%%` inside a quotation
// as a splice. So the operator either ends the line the left-hand side is on, or takes a line of
// its own one level in. Working down from the shortest case:
//
//     1. everything fits           lhs op rhs
//
//     2. the rhs does not fit      lhs op
//                                      rhs
//
//     3. the lhs spans lines       lhs
//                                      op
//                                      rhs
//
// The third is what both style guides ask for in a long function signature, where the parameters
// are indented one level and the `=` takes a line of its own before the body. Under
// `fsharp_multiline_bracket_style = stroustrup` a right-hand side that opens a bracket keeps
// hugging the operator, which outranks the third case.

[<Test>]
let ``= keeps the whole expression on one line when it fits`` () =
    formatSourceString
        """
let v = xs = ys
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v = xs = ys
"""

[<Test>]
let ``= moves the right-hand side down one level when it does not fit`` () =
    formatSourceString
        """
let v = xs = [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; "ffffffffff" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs =
        [
            "aaaaaaaaaa"
            "bbbbbbbbbb"
            "cccccccccc"
            "dddddddddd"
            "eeeeeeeeee"
            "ffffffffff"
        ]
"""

[<Test>]
let ``= takes a line of its own when the left-hand side is multiline`` () =
    formatSourceString
        """
let v = someFunction aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc = 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    someFunction
        aaaaaaaaaaaaaaaaaaaaaa
        bbbbbbbbbbbbbbbbbbbbbb
        cccccccccccccccccccccccccccc
        =
        0
"""

[<Test>]
let ``> keeps the whole expression on one line when it fits`` () =
    formatSourceString
        """
let v = xs > ys
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v = xs > ys
"""

[<Test>]
let ``> moves the right-hand side down one level when it does not fit`` () =
    formatSourceString
        """
let v = xs > [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; "ffffffffff" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs >
        [
            "aaaaaaaaaa"
            "bbbbbbbbbb"
            "cccccccccc"
            "dddddddddd"
            "eeeeeeeeee"
            "ffffffffff"
        ]
"""

[<Test>]
let ``> takes a line of its own when the left-hand side is multiline`` () =
    formatSourceString
        """
let v = someFunction aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc > 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    someFunction
        aaaaaaaaaaaaaaaaaaaaaa
        bbbbbbbbbbbbbbbbbbbbbb
        cccccccccccccccccccccccccccc
        >
        0
"""

[<Test>]
let ``< keeps the whole expression on one line when it fits`` () =
    formatSourceString
        """
let v = xs < ys
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v = xs < ys
"""

[<Test>]
let ``< moves the right-hand side down one level when it does not fit`` () =
    formatSourceString
        """
let v = xs < [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; "ffffffffff" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs <
        [
            "aaaaaaaaaa"
            "bbbbbbbbbb"
            "cccccccccc"
            "dddddddddd"
            "eeeeeeeeee"
            "ffffffffff"
        ]
"""

[<Test>]
let ``< takes a line of its own when the left-hand side is multiline`` () =
    formatSourceString
        """
let v = someFunction aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc < 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    someFunction
        aaaaaaaaaaaaaaaaaaaaaa
        bbbbbbbbbbbbbbbbbbbbbb
        cccccccccccccccccccccccccccc
        <
        0
"""

[<Test>]
let ``% keeps the whole expression on one line when it fits`` () =
    formatSourceString
        """
let v = xs % ys
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v = xs % ys
"""

[<Test>]
let ``% moves the right-hand side down one level when it does not fit`` () =
    formatSourceString
        """
let v = xs % [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; "ffffffffff" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs %
        [
            "aaaaaaaaaa"
            "bbbbbbbbbb"
            "cccccccccc"
            "dddddddddd"
            "eeeeeeeeee"
            "ffffffffff"
        ]
"""

[<Test>]
let ``% takes a line of its own when the left-hand side is multiline`` () =
    formatSourceString
        """
let v = someFunction aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc % 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    someFunction
        aaaaaaaaaaaaaaaaaaaaaa
        bbbbbbbbbbbbbbbbbbbbbb
        cccccccccccccccccccccccccccc
        %
        0
"""

[<Test>]
let ``%% keeps the whole expression on one line when it fits`` () =
    formatSourceString
        """
let v = xs %% ys
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v = xs %% ys
"""

[<Test>]
let ``%% moves the right-hand side down one level when it does not fit`` () =
    formatSourceString
        """
let v = xs %% [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeeeee"; "ffffffffff" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs %%
        [
            "aaaaaaaaaa"
            "bbbbbbbbbb"
            "cccccccccc"
            "dddddddddd"
            "eeeeeeeeee"
            "ffffffffff"
        ]
"""

[<Test>]
let ``%% takes a line of its own when the left-hand side is multiline`` () =
    formatSourceString
        """
let v = someFunction aaaaaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbbbbbb cccccccccccccccccccccccccccc %% 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    someFunction
        aaaaaaaaaaaaaaaaaaaaaa
        bbbbbbbbbbbbbbbbbbbbbb
        cccccccccccccccccccccccccccc
        %%
        0
"""

[<Test>]
let ``a right-hand side that opens a bracket starts at the same column as any other`` () =
    formatSourceString
        """
let v = xs = { XXXX = 1; YYYY = 2; ZZZZ = 3; WWWW = 4; VVVV = 5; TTTT = "sevenseven" }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs =
        {
            XXXX = 1
            YYYY = 2
            ZZZZ = 3
            WWWW = 4
            VVVV = 5
            TTTT = "sevenseven"
        }
"""

[<Test>]
let ``a match on the right-hand side does not follow the left-hand side`` () =
    formatSourceString
        """
let v = xs = match yyyyyyyyyyyyyyyyy with | Aaaaaaaaaaaaaaaaaaaa -> 1 | Bbbbbbbbbb -> 2
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs =
        match yyyyyyyyyyyyyyyyy with
        | Aaaaaaaaaaaaaaaaaaaa -> 1
        | Bbbbbbbbbb -> 2
"""

[<Test>]
let ``the name on the left does not decide where the right-hand side starts`` () =
    formatSourceString
        """
let v = xs = match yyyyyyyyyyyyyyyyy with | Aaaaaaaaaaaaaaaaaaaa -> 1 | Bbbbbbbbbb -> 2
let w = xsy = match yyyyyyyyyyyyyyyyy with | Aaaaaaaaaaaaaaaaaaaa -> 1 | Bbbbbbbbbb -> 2
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs =
        match yyyyyyyyyyyyyyyyy with
        | Aaaaaaaaaaaaaaaaaaaa -> 1
        | Bbbbbbbbbb -> 2

let w =
    xsy =
        match yyyyyyyyyyyyyyyyy with
        | Aaaaaaaaaaaaaaaaaaaa -> 1
        | Bbbbbbbbbb -> 2
"""

[<Test>]
let ``both sides multiline puts the operator between them`` () =
    formatSourceString
        """
let v = xs.ReplaceEverythingEverywhere(11111, 22222).ReplaceEverythingEverywhere(33333, 44444).TrimEnd() = expected.ReplaceEverythingEverywhere(55555, 66666).ReplaceEverythingEverywhere(77777, 88888).TrimEnd()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs
        .ReplaceEverythingEverywhere(11111, 22222)
        .ReplaceEverythingEverywhere(33333, 44444)
        .TrimEnd()
        =
        expected
            .ReplaceEverythingEverywhere(55555, 66666)
            .ReplaceEverythingEverywhere(77777, 88888)
            .TrimEnd()
"""

[<Test>]
let ``a comment in front of the right-hand side lands below the operator`` () =
    formatSourceString
        """
let v =
    xs =
        // a comment
        [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeee" ]
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs =
        // a comment
        [ "aaaaaaaaaa"; "bbbbbbbbbb"; "cccccccccc"; "dddddddddd"; "eeeeeeee" ]
"""

[<Test>]
let ``a bracket keeps hugging the operator under stroustrup`` () =
    formatSourceString
        """
let v = xs = [ "aaaaaaaaaaaaaaaaaa"; "bbbbbbbbbbbbbbbbbb"; "cccccccccccccccccc"; "dddddddddddddddddd" ]
"""
        { config with
            MaxLineLength = 80
            MultilineBracketStyle = Stroustrup
        }
    |> prepend newline
    |> should
        equal
        """
let v =
    xs = [
        "aaaaaaaaaaaaaaaaaa"
        "bbbbbbbbbbbbbbbbbb"
        "cccccccccccccccccc"
        "dddddddddddddddddd"
    ]
"""

[<Test>]
let ``the stroustrup hug outranks the operator taking its own line`` () =
    formatSourceString
        """
let long = xs.ReplaceEverythingEverywhere(11111, 22222).ReplaceEverythingEverywhere(33333, 44444).TrimEnd() = [ "aaaaaaaaaaaaaaaaaaaaaa"; "bbbbbbbbbbbbbbbbbbbbbb"; "cccccccccccccccccccccc"; "dddddddddddddddddddddd" ]
"""
        { config with
            MaxLineLength = 80
            MultilineBracketStyle = Stroustrup
        }
    |> prepend newline
    |> should
        equal
        """
let long =
    xs
        .ReplaceEverythingEverywhere(11111, 22222)
        .ReplaceEverythingEverywhere(33333, 44444)
        .TrimEnd() = [
        "aaaaaaaaaaaaaaaaaaaaaa"
        "bbbbbbbbbbbbbbbbbbbbbb"
        "cccccccccccccccccccccc"
        "dddddddddddddddddddddd"
    ]
"""

[<Test>]
let ``an infix percent inside a quotation does not become a splice`` () =
    formatSourceString
        """
let q = <@ someLongThingHereOk aaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbb % otherThingLong @>
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let q =
    <@
        someLongThingHereOk aaaaaaaaaaaaaaaaaaa bbbbbbbbbbbbbbbbbb %
            otherThingLong
    @>
"""
