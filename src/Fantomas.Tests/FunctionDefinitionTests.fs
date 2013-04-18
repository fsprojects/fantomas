module Fantomas.Tests.FunctionDefinitionTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``recursive functions``() =
    formatSourceString false """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x

and g x = x
"""

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

[<Test>]
let ``should keep identifiers with whitespace in double backticks``() =
    formatSourceString false """let ``should keep identifiers in double backticks``() = x
    """ config
    |> should equal """let ``should keep identifiers in double backticks``() = x
"""

[<Test>]
let ``should remove backticks from shouldn't identifier``() =
    formatSourceString false """let ``shouldn't``() = x
    """ config
    |> should equal """let shouldn't() = x
"""

[<Test>]
let ``should keep identifiers with + in double backticks``() =
    formatSourceString false """let ``Foo+Bar``() = x
    """ config
    |> should equal """let ``Foo+Bar``() = x
"""