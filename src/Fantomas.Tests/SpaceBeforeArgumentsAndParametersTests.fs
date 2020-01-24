module Fantomas.Tests.SpaceBeforeArgumentsAndParametersTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

/// Space before () in Uppercase function call

[<Test>]
let ``default config should not add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" config
    |> should equal """let value = MyFunction()
"""

[<Test>]
let ``SpaceBeforeUnitArgumentInUppercaseFunctionCall should add space before unit in uppercase function call`` () =
    formatSourceString false "let value = MyFunction()" ({ config with SpaceBeforeUnitArgumentInUppercaseFunctionCall = true })
    |> should equal """let value = MyFunction ()
"""

[<Test>]
let ``SpaceBeforeUnitArgumentInUppercaseFunctionCall should add space before unit in chained uppercase function call`` () =
    formatSourceString false "let value = person.ToString()" ({ config with SpaceBeforeUnitArgumentInUppercaseFunctionCall = true })
    |> should equal """let value = person.ToString ()
"""

/// Space before () in lowercase function call

[<Test>]
let ``default config should not add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" config
    |> should equal """let value = myFunction()
"""

[<Test>]
let ``SpaceBeforeUnitArgumentInLowercaseFunctionCall should add space before unit in lowercase function call`` () =
    formatSourceString false "let value = myFunction()" ({ config with SpaceBeforeUnitArgumentInLowercaseFunctionCall = true })
    |> should equal """let value = myFunction ()
"""

// Exception to the rule

[<Test>]
let ``SpaceBeforeUnitArgumentInUppercaseFunctionCall and SpaceBeforeUnitArgumentInLowercaseFunctionCall should not have impact when member is called after unit`` () =
    formatSourceString false """let v1 = myFunction().Member
let v2 = OtherFunction().Member
"""  ({ config with
            SpaceBeforeUnitArgumentInUppercaseFunctionCall = true
            SpaceBeforeUnitArgumentInLowercaseFunctionCall = true })
    |> prepend newline
    |> should equal """
let v1 = myFunction().Member
let v2 = OtherFunction().Member
"""

// Space before parenthesis (a+b) in Uppercase function call

[<Test>]
let ``default config should not add space before parenthesis in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" config
    |> should equal """let value = MyFunction(a + b)
"""

[<Test>]
let ``SpaceBeforeParenthesisArgumentInUppercaseFunctionCall should add space before parenthesis in uppercase function call`` () =
    formatSourceString false "let value = MyFunction(a+b)" ({ config with SpaceBeforeParenthesisArgumentInUppercaseFunctionCall = true })
    |> should equal """let value = MyFunction (a + b)
"""

// Space before parenthesis (a+b) in lowercase function call

[<Test>]
let ``default config should add space before parenthesis in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" config
    |> should equal """let value = myFunction (a + b)
"""

[<Test>]
let ``SpaceBeforeParenthesisArgumentInUppercaseFunctionCall = false, should not add space before parenthesis in lowercase function call`` () =
    formatSourceString false "let value = myFunction(a+b)" ({ config with SpaceBeforeParenthesisArgumentInLowercaseFunctionCall = false })
    |> should equal """let value = myFunction(a + b)
"""

// Space before unit in Uppercase function signature

[<Test>]
let ``default config should not add space before unit in uppercase function definition`` () =
    formatSourceString false "let Value () = x" config
    |> should equal """let Value() = x
"""

[<Test>]
let ``SpaceBeforeUnitParameterInUppercaseFunctionDefinition config should not add space before unit in uppercase function definition`` () =
    formatSourceString false "let Value() = x" ({ config with SpaceBeforeUnitParameterInUppercaseFunctionDefinition = true })
    |> should equal """let Value () = x
"""

[<Test>]
let ``default config should not add space after empty constructor of class`` () =
    formatSourceString false """type Person () =
    class end
"""  config
    |> prepend newline
    |> should equal """
type Person() =
    class
    end
"""

[<Test>]
let ``default config should not add space after constructor of class`` () =
    formatSourceString false """type Person (a:int) =
    class end
"""  config
    |> prepend newline
    |> should equal """
type Person(a: int) =
    class
    end
"""

[<Test>]
let ``SpaceBeforeUnitParameterInUppercaseFunctionDefinition should add space after constructor of class`` () =
    formatSourceString false """type Person () =
    class end

type Animal (length:int) =
    class end
"""  ({ config with SpaceBeforeUnitParameterInUppercaseFunctionDefinition = true })
    |> prepend newline
    |> should equal """
type Person() =
    class
    end

type Animal(length: int) =
    class
    end
"""

// Space before unit in lowercase function definition

[<Test>]
let ``default config should not add space before unit in lowercase function definition`` () =
    formatSourceString false "let value () = x" config
    |> should equal """let value() = x
"""

[<Test>]
let ``SpaceBeforeUnitParameterInLowercaseFunctionDefinition config should add space before unit in lowercase function definition`` () =
    formatSourceString false "let value() = x" ({ config with SpaceBeforeUnitParameterInLowercaseFunctionDefinition = true })
    |> should equal """let value () = x
"""

// Space before parenthesis (a+b) in Uppercase function definition

[<Test>]
let ``default config should not add space before parenthesis in uppercase function definition`` () =
    formatSourceString false "let Value (a:int) = x" config
    |> should equal """let Value(a: int) = x
"""

[<Test>]
let ``SpaceBeforeParenthesisInUppercaseFunctionDefinition config should add space before parenthesis in uppercase function definition`` () =
    formatSourceString false "let Value(a:int) = x" ({ config with SpaceBeforeParenthesisInUppercaseFunctionDefinition = true })
    |> should equal """let Value (a: int) = x
"""

// Space before parenthesis (a+b) in lowercase function definition

[<Test>]
let ``default config should not add space before parenthesis in lowercase function definition`` () =
    formatSourceString false "let value (a:int) = x" config
    |> should equal """let value (a: int) = x
"""

[<Test>]
let ``SpaceBeforeParenthesisInLowercaseFunctionDefinition = false, should not add space before parenthesis in lowercase function definition`` () =
    formatSourceString false "let value(a:int) = x" ({ config with SpaceBeforeParenthesisInLowercaseFunctionDefinition = false })
    |> should equal """let value(a: int) = x
"""