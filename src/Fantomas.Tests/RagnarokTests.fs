module Fantomas.Tests.RagnarokTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          MultilineBlockBracketsOnSameColumn = true
          Ragnarok = true }

[<Test>]
let ``synbinding value with record instance `` () =
    formatSourceString
        false
        """
let x =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding function with record instance `` () =
    formatSourceString
        false
        """
let x y =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding value with anonymous record instance `` () =
    formatSourceString
        false
        """
let x =
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding value with anonymous record instance struct`` () =
    formatSourceString
        false
        """
let x =
   struct
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""