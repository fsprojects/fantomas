module Fantomas.Core.Tests.DotSetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``function application with parentheses should not respect SpaceBeforeUppercaseInvocation`` () =
    formatSourceString
        false
        """
c.P.Add(x).Value <- v
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
c.P.Add(x).Value <- v
"""

[<Test>]
let ``function application with parentheses should remain idempotent, 2549`` () =
    formatSourceString
        false
        """
let foo =
    c.P.Add(NpgsqlParameter ("day", NpgsqlTypes.NpgsqlDbType.Date)).Value <- query.Day.Date
    "fooo"
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let foo =
    c.P.Add(NpgsqlParameter ("day", NpgsqlTypes.NpgsqlDbType.Date)).Value <- query.Day.Date
    "fooo"
"""

[<Test>]
let ``dotSet with unit param on lhs`` () =
    formatSourceString
        false
        """
app().foo <- thing
"""
        { config with
            SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
app().foo <- thing
"""

[<Test>]
let ``dotSet with DotGet then unit param on lhs`` () =
    formatSourceString
        false
        """
app.last().foo <- foo().thing.other().thing
"""
        { config with
            SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
app.last().foo <- foo().thing.other().thing
"""

[<Test>]
let ``bad format result with SynExpr.DotSet, 2000`` () =
    formatSourceString
        false
        """
app().foo <- {|
    X = 45
    Y =
        // comment
        79
    // zzzz
    Z = 230
|}
"""
        { config with
            SpaceBeforeLowercaseInvocation = true }
    |> fun formatted ->
        formatSourceString
            false
            formatted
            { config with
                SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
app().foo <-
    {| X = 45
       Y =
        // comment
        79
       // zzzz
       Z = 230 |}
"""
