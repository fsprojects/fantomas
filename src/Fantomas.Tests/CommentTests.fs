module Fantomas.Tests.CommentTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep // comments after nowarn directives``() =
    formatSourceString false """#nowarn "51" // address-of operator can occur in the code
    """ config
    |> should equal """#nowarn "51" // address-of operator can occur in the code
"""
