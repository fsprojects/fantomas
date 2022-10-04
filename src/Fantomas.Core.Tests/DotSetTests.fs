module Fantomas.Core.Tests.DotSetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``function application with parentheses should not respect SpaceBeforeUppercaseInvocation`` () =
    formatSourceString
        false
        """
c.P.Add(x).Value <- v
"""
        { config with SpaceBeforeUppercaseInvocation = true }
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
        { config with SpaceBeforeUppercaseInvocation = true }
    |> fun formatted -> formatSourceString false formatted { config with SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
let foo =
    c.P.Add(NpgsqlParameter ("day", NpgsqlTypes.NpgsqlDbType.Date)).Value <- query.Day.Date
    "fooo"
"""
