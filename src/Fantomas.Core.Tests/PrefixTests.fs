module Fantomas.Core.Tests.PrefixTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

[<Test>]
let ``should format prefix operators`` () =
    formatSourceString
        """let x = -y
let z = !!x
    """
        config
    |> should
        equal
        """let x = -y
let z = !!x
"""

[<Test>]
let ``should keep triple ~~~ operator`` () =
    formatSourceString
        """x ~~~FileAttributes.ReadOnly
    """
        config
    |> should
        equal
        """x ~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``should keep single triple ~~~ operator`` () =
    formatSourceString
        """~~~FileAttributes.ReadOnly
    """
        config
    |> should
        equal
        """~~~FileAttributes.ReadOnly
"""

[<Test>]
let ``operator before verbatim string add extra space, 736`` () =
    formatSourceString
        """Target M.Tools (fun _ -> !! @"Tools\Tools.sln" |> rebuild)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Target M.Tools (fun _ -> !! @"Tools\Tools.sln" |> rebuild)
"""

[<Test>]
let ``should keep parens around !+ prefix operator definition`` () =
    formatSourceString
        """let (!+) x = Include x
    """
        config
    |> should
        equal
        """let (!+) x = Include x
"""

[<Test>]
let ``adding space after prefix operator breaks code, 2796`` () =
    formatSourceString
        """
let inline (~%%) id = int id

let f a b = a + b

let foo () = f %%"17" %%"42"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline (~%%) id = int id

let f a b = a + b

let foo () = f %%"17" %%"42"
"""

[<Test>]
let ``tilde unary operator with literal and variable, 3131`` () =
    formatSourceString
        """
let x = ~~1
let y = ~~x
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = ~~1
let y = ~~x
"""

[<Test>]
let ``prefix application with interpolated string, 1414`` () =
    formatSourceString
        """
!- $".{s}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
!- $".{s}"
"""

let operator_application_literal_values_with_sign =
    [ "-86y"
      "-86s"
      "-86"
      "-86l"
      "-123n"
      "-86L"
      "-4.41F"
      "-4.14"
      "-12456I"
      "-0.7833M" ]

[<TestCaseSource("operator_application_literal_values_with_sign")>]
let ``operators maintain spacing from literal values which start with + or -`` (literalValue: string) =
    formatSourceString
        $"""
let subtractTwo = + %s{literalValue}
"""
        config
    |> prepend newline
    |> should
        equal
        $"""
let subtractTwo = + %s{literalValue}
"""

let operator_application_literal_values_without_sign =
    [ "86uy"
      "86us"
      "86u"
      "86ul"
      "0x00002D3Fun"
      "86UL"
      "'a'"
      "\"text\""
      "'a'B"
      "\"text\"B" ]

[<TestCaseSource("operator_application_literal_values_without_sign")>]
let ``operators maintain spacing from literal values which start without + or -`` (literalValue: string) =
    formatSourceString
        $"""
let subtractTwo = + %s{literalValue}
"""
        config
    |> prepend newline
    |> should
        equal
        $"""
let subtractTwo = +%s{literalValue}
"""
