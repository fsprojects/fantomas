module Fantomas.Tests.CastTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``multiline downcast expression, `` () =
    formatSourceString false """
longMethodName
    longArgument
    longArgument2
:?> List<bool>
"""
        { config with
              MaxLineLength = 30
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              DisableElmishSyntax = true }
    |> prepend newline
    |> should equal """
longMethodName
    longArgument
    longArgument2
:?> List<bool>
"""

[<Test>]
let ``multiline upcast expression, `` () =
    formatSourceString false """
longMethodName
    longArgument
    longArgument2
:> List<bool>
"""
        { config with
              MaxLineLength = 30
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              DisableElmishSyntax = true }
    |> prepend newline
    |> should equal """
longMethodName
    longArgument
    longArgument2
:> List<bool>
"""
