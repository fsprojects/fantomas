module Fantomas.Tests.SpaceBeforeUppercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let spaceBeforeConfig = { config with SpaceBeforeUppercaseInvocation = true }

/// Space before () in Uppercase function call

[<Test>]
let ``default config should not add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" config
    |> should equal """let value = MyFunction()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" spaceBeforeConfig
    |> should equal """let value = MyFunction ()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in chained uppercase function call`` () =
    formatSourceString false "let value = person.ToString()" spaceBeforeConfig
    |> should equal """let value = person.ToString ()
"""

// Exception to the rule

[<Test>]
let ``spaceBeforeUppercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString false "let v2 = OtherFunction().Member"  spaceBeforeConfig
    |> prepend newline
    |> should equal """
let v2 = OtherFunction().Member
"""

// Space before parentheses (a+b) in Uppercase function call

[<Test>]
let ``default config should not add space before parentheses in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" config
    |> should equal """let value = MyFunction(a + b)
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before parentheses in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" spaceBeforeConfig
    |> should equal """let value = MyFunction (a + b)
"""

[<Test>]
let ``space before uppercase function application cannot apply with dot-chaining, 943`` () =
    formatSourceString false """foo.Bar().[5]
"""  { config with SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should equal """
foo.Bar().[5]
"""

[<Test>]
let ``space before uppercase DotIndexedSet`` () =
    formatSourceString false """foo.Bar().[5] <- 5
"""  { config with SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should equal """
foo.Bar().[5] <- 5
"""

[<Test>]
let ``setting SpaceBeforeUppercaseInvocation is not applied in the middle of a invocation chain, 853`` () =
    formatSourceString false """
module SomeModule =
    let DoSomething (a:SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""  { config with SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should equal """
module SomeModule =
    let DoSomething (a: SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""
