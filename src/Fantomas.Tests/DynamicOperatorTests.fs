module Fantomas.Tests.DynamicOperatorTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``Keep () when dynamic operator is used`` () =
    formatSourceString false "let memoEquals x = x?(k + 1)" config
    |> should
        equal
        """let memoEquals x = x?(k + 1)
"""

[<Test>]
let ``Remove () when dynamic operator is string`` () =
    formatSourceString false "let memoEquals x = x?k" config
    |> should
        equal
        """let memoEquals x = x?k
"""

[<Test>]
let ``keep () when dynamic operator inside boolean expr, #476`` () =
    formatSourceString
        false
        """let fieldColor (fieldNameX: string) =
    if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
        IsDanger
    else
        NoColor
    |> Input.Color
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fieldColor (fieldNameX: string) =
    (if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
         IsDanger
     else
         NoColor)
    |> Input.Color
"""

[<Test>]
let ``preserve back ticks from checked keyword, 937`` () =
    formatSourceString false "let toggle = unbox<bool> (e.target?``checked``)" config
    |> prepend newline
    |> should
        equal
        """
let toggle = unbox<bool> (e.target?``checked``)
"""
