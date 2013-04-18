module Fantomas.Tests.FunctionDefinitionTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep mutually recursive functions``() =
    formatSourceString false """
let rec createJArray x = createJObject

and createJObject y = createJArray
    """ config
    |> should equal """let rec createJArray x = createJObject

and createJObject y = createJArray
"""

[<Test>]
let ``should keep mutually recursive functions in nested function``() =
    formatSourceString false """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
    """ config
    |> should equal """let f = 
    let rec createJArray x = createJObject x
    and createJObject y = createJArray y
    createJArray
"""