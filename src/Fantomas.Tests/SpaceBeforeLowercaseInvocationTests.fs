module Fantomas.Tests.SpaceBeforeLowercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let noSpaceBefore = { config with SpaceBeforeLowercaseInvocation = false }

/// Space before () in lowercase function call

[<Test>]
let ``default config should add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" config
    |> should equal """let value = myFunction ()
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" noSpaceBefore
    |> should equal """let value = myFunction()
"""

// Space before parentheses (a+b) in lowercase function call

[<Test>]
let ``default config should add space before parentheses in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" config
    |> should equal """let value = myFunction (a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before parentheses in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" noSpaceBefore
    |> should equal """let value = myFunction(a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString false "let v1 = myFunction().Member" noSpaceBefore
    |> prepend newline
    |> should equal """
let v1 = myFunction().Member
"""