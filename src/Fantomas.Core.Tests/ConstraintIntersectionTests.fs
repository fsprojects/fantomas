module Fantomas.Core.Tests.ConstraintIntersectionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``constraint intersection in type annotation`` () =
    formatSourceString
        false
        """
let y (f: #I & #Task<int> & #seq<string>) = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y (f: #I & #Task<int> & #seq<string>) = ()
"""

[<Test>]
let ``constraint intersection with leading typar`` () =
    formatSourceString
        false
        """
let y (f: 't & #I & #IDisposable & #seq<int> & #I2) = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y (f: 't & #I & #IDisposable & #seq<int> & #I2) = ()
"""

[<Test>]
let ``usage in member`` () =
    formatSourceString
        false
        """
type I =
    abstract h: #IDisposable & #seq<int> & #I -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
type I =
    abstract h: #IDisposable & #seq<int> & #I -> unit
"""
