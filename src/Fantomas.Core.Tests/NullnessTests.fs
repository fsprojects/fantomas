module Fantomas.Core.Tests.NullnessTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``du case of string or null`` () =
    formatSourceString
        """
type DU = MyCase of (string | null)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type DU = MyCase of (string | null)
"""

[<Test>]
let ``multiple or type`` () =
    formatSourceString
        """
let myFunc ("abc" | "" : string | null | "123") = 15
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myFunc ("abc" | "": string | null | "123") = 15
"""

[<Test>]
let ``null type constraint`` () =
    formatSourceString
        """
let myFunc() : 'T when 'T : not struct and 'T:null = null
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myFunc () : 'T when 'T: not struct and 'T: null = null
"""

[<Test>]
let ``not null type constraint`` () =
    formatSourceString
        """
let myFunc (x: 'T when 'T: not null) = 42
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myFunc (x: 'T when 'T: not null) = 42
"""

[<Test>]
let ``not null in type constraints`` () =
    formatSourceString
        """
type C<'T when 'T: not null> = class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type C<'T when 'T: not null> = class end
"""

[<Test>]
let ``or null pattern`` () =
    formatSourceString
        """
match x with
| :? string | null -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| :? string
| null -> ()
"""

[<Test>]
let ``nullness in signature file`` () =
    formatSignatureString
        """
namespace Meh

type DU = MyCase of (string | null)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Meh

type DU = MyCase of (string | null)
"""

[<Test>]
let ``trivia in SynType.WithNull`` () =
    formatSourceString
        """
type DU = MyCase of (string 
                        | // but why?
                            null)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type DU =
    | MyCase of
        (string | // but why?
            null)
"""

[<Test>]
let ``trivia in SynTypeConstraint.WhereTyparNotSupportsNull`` () =
    formatSourceString
        """
type C<'T when 
                'T
                    : // comment 1 
                    not // comment 2
                        null> = class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type C<'T
    when 'T: // comment 1
        not // comment 2
        null> = class end
"""
