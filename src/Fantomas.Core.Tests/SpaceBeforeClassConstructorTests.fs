module Fantomas.Core.Tests.SpaceBeforeClassConstructorTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let spaceBeforeConfig =
    { config with
        SpaceBeforeClassConstructor = true }

// Space before unit in Uppercase class definition

[<Test>]
let ``default config should not add space before unit in uppercase class definition`` () =
    formatSourceString "type Person () = class end" config
    |> should
        equal
        """type Person() = class end
"""

[<Test>]
let ``SpaceBeforeUnitParameterInUppercaseClassConstructor should add space after constructor of class`` () =
    formatSourceString
        """type Person () =
    class end
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type Person () = class end
"""

// Space before unit in lowercase class definition

[<Test>]
let ``default config should not add space before unit in lowercase class definition`` () =
    formatSourceString
        """type t () =
    class
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type t() = class end
"""

[<Test>]
let ``SpaceBeforeUnitParameterInLowercaseClassConstructor should add space before unit in lowercase class definition``
    ()
    =
    formatSourceString
        """type t() =
    class end
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type t () = class end
"""

// Space before parentheses in Uppercase class definition

[<Test>]
let ``default config should not add space before uppercase constructor of class`` () =
    formatSourceString
        """
type Animal(length:int) =
    class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Animal(length: int) = class end
"""

[<Test>]
let ``SpaceBeforeParenthesisParameterInUppercaseClassConstructor should add space before uppercase constructor of class``
    ()
    =
    formatSourceString
        """
type Animal(length:int) =
    class end
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type Animal (length: int) = class end
"""

// Space before parentheses in lowercase class definition

[<Test>]
let ``default config should not add space before lowercase constructor of class`` () =
    formatSourceString
        """
type animal(length:int) =
    class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type animal(length: int) = class end
"""

[<Test>]
let ``SpaceBeforeParenthesisParameterInLowercaseClassConstructor should add space before lowercase constructor of class``
    ()
    =
    formatSourceString
        """
type animal(length:int) =
    class end
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type animal (length: int) = class end
"""

// Space before parentheses in secondary class constructor

[<Test>]
let ``should add space before secondary constructor of class declared with new, 964`` () =
    formatSourceString
        """
type animal (length: int) =
    new(length) = animal (length)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type animal (length: int) =
    new (length) = animal (length)
"""

[<Test>]
let ``should add space after inherit base class declaration, 964`` () =
    formatSourceString
        """
type dog() =
    inherit animal()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type dog () =
    inherit animal ()
"""

[<Test>]
let ``should add space before new and inherit on constructor of class, 964`` () =
    formatSourceString
        """
type ProtocolGlitchException =
    inherit CommunicationUnsuccessfulException

    new(message) = { inherit CommunicationUnsuccessfulException(message) }

    new(message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException(message, innerException) }
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type ProtocolGlitchException =
    inherit CommunicationUnsuccessfulException

    new (message) = { inherit CommunicationUnsuccessfulException (message) }

    new (message: string, innerException: Exception) =
        { inherit CommunicationUnsuccessfulException (message, innerException) }
"""

[<Test>]
let ``should add space before new and inherit on constructor of class with multiline record, 964`` () =
    formatSourceString
        """
type BaseClass =
    val string1: string
    new(str) = { string1 = str }
    new() = { string1 = "" }

type DerivedClass =
    inherit BaseClass

    val string2: string

    new(str1, str2) =
        { inherit BaseClass(str1)
          string2 = str2 }

    new(str2) = { inherit BaseClass(); string2 = str2 }
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
type BaseClass =
    val string1: string
    new (str) = { string1 = str }
    new () = { string1 = "" }

type DerivedClass =
    inherit BaseClass

    val string2: string

    new (str1, str2) =
        { inherit BaseClass (str1)
          string2 = str2 }

    new (str2) = { inherit BaseClass (); string2 = str2 }
"""

[<Test>]
let ``should add space before inherit on constructor that takes a constant value`` () =
    formatSourceString
        """
type DerivedClass =
    inherit BaseClass

    val string2: string

    new (str1, str2) =
        { inherit BaseClass "meh"
          string2 = str2 }
"""
        { config with
            SpaceBeforeClassConstructor = false }
    |> prepend newline
    |> should
        equal
        """
type DerivedClass =
    inherit BaseClass

    val string2: string

    new(str1, str2) =
        { inherit BaseClass "meh"
          string2 = str2 }
"""

[<Test>]
let ``should add space before inherit on constructor of class with multiline record, MultilineBlockBracketsOnSameColumn``
    ()
    =
    formatSourceString
        """
type DerivedClass =
    inherit BaseClass

    val string2: string

    new(str1, str2) =
        { inherit BaseClass(str1)
          string2 = str2 }
"""
        { spaceBeforeConfig with
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
type DerivedClass =
    inherit BaseClass

    val string2: string

    new (str1, str2) =
        {
            inherit BaseClass (str1)
            string2 = str2
        }
"""
