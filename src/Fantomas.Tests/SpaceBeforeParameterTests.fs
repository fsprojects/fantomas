module Fantomas.Tests.SpaceBeforeParameterTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let noSpaceBefore = { config with SpaceBeforeParameter = false }

// Space before unit in Uppercase function signature

[<Test>]
let ``default config should add space before unit in uppercase function definition`` () =
    formatSourceString false "let Value () = x" config
    |> should equal """let Value () = x
"""

[<Test>]
let ``noSpaceBefore = false, should not add space before unit in uppercase function definition`` () =
    formatSourceString false "let Value() = x" noSpaceBefore
    |> should equal """let Value() = x
"""

// Space before unit in lowercase function definition

[<Test>]
let ``default config should add space before unit in lowercase function definition`` () =
    formatSourceString false "let value () = x" config
    |> should equal """let value () = x
"""

[<Test>]
let ``spaceBeforeParameter = false, should not add space before unit in lowercase function definition`` () =
    formatSourceString false "let value() = x" noSpaceBefore
    |> should equal """let value() = x
"""

// Space before parentheses (a+b) in Uppercase function definition

[<Test>]
let ``default config should add space before parentheses in uppercase function definition`` () =
    formatSourceString false "let Value (a:int) = x" config
    |> should equal """let Value (a: int) = x
"""

[<Test>]
let ``spaceBeforeParameter = false, should not add space before parentheses in uppercase function definition`` () =
    formatSourceString false "let Value(a:int) = x" noSpaceBefore
    |> should equal """let Value(a: int) = x
"""

[<Test>]
let ``default config should add space after discrimintation union member`` () =
    formatSourceString false """match x with
| Zero() -> ()
| One (o) -> ()
| Two(o,t) -> ()
"""  config
    |> prepend newline
    |> should equal """
match x with
| Zero () -> ()
| One (o) -> ()
| Two (o, t) -> ()
"""

// Space before parentheses (a+b) in lowercase function definition

[<Test>]
let ``default config should add space before parentheses in lowercase function definition`` () =
    formatSourceString false "let value(a:int) = x" config
    |> should equal """let value (a: int) = x
"""

[<Test>]
let ``spaceBeforeParameter = false, should not add space before parentheses in lowercase function definition`` () =
    formatSourceString false "let value (a:int) = x" noSpaceBefore
    |> should equal """let value(a: int) = x
"""
