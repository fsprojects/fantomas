module Fantomas.Tests.SpaceBeforeClassConstructorTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let spaceBeforeConfig = { config with SpaceBeforeClassConstructor = true }

// Space before unit in Uppercase class definition

[<Test>]
let ``default config should not add space before unit in uppercase class definition`` () =
    formatSourceString false "type Person () = class end" config
    |> should equal """type Person() =
    class
    end
"""

[<Test>]
let ``SpaceBeforeUnitParameterInUppercaseClassConstructor should add space after constructor of class`` () =
    formatSourceString false """type Person () =
    class end
"""  spaceBeforeConfig
    |> prepend newline
    |> should equal """
type Person () =
    class
    end
"""

// Space before unit in lowercase class definition

[<Test>]
let ``default config should not add space before unit in lowercase class definition`` () =
    formatSourceString false """type t () =
    class
    end
"""  config
    |> prepend newline
    |> should equal """
type t() =
    class
    end
"""

[<Test>]
let ``SpaceBeforeUnitParameterInLowercaseClassConstructor should add space before unit in lowercase class definition`` () =
    formatSourceString false """type t() =
    class
    end
"""  spaceBeforeConfig
    |> prepend newline
    |> should equal """
type t () =
    class
    end
"""

// Space before parentheses in Uppercase class definition

[<Test>]
let ``default config should not add space before uppercase constructor of class`` () =
    formatSourceString false """
type Animal(length:int) =
    class end
"""  config
    |> prepend newline
    |> should equal """
type Animal(length: int) =
    class
    end
"""

[<Test>]
let ``SpaceBeforeParenthesisParameterInUppercaseClassConstructor should add space before uppercase constructor of class`` () =
    formatSourceString false """
type Animal(length:int) =
    class end
"""  spaceBeforeConfig
    |> prepend newline
    |> should equal """
type Animal (length: int) =
    class
    end
"""

// Space before parentheses in lowercase class definition

[<Test>]
let ``default config should not add space before lowercase constructor of class`` () =
    formatSourceString false """
type animal(length:int) =
    class end
"""  config
    |> prepend newline
    |> should equal """
type animal(length: int) =
    class
    end
"""

[<Test>]
let ``SpaceBeforeParenthesisParameterInUppercaseClassConstructor should add space before lowercase constructor of class`` () =
    formatSourceString false """
type animal(length:int) =
    class end
"""  spaceBeforeConfig
    |> prepend newline
    |> should equal """
type animal (length: int) =
    class
    end
"""
