module Fantomas.Tests.QuotationTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``typed quotations``() =
    formatSourceString false """
    <@ 
        let f x = x + 10
        f 20
    @>""" config
    |> prepend newline
    |> should equal """
<@ let f x = x + 10
   f 20 @>
"""

[<Test>]
let ``untyped quotations``() =
    formatSourceString false "<@@ 2 + 3 @@>" config
    |> should equal """<@@ 2 + 3 @@>
"""