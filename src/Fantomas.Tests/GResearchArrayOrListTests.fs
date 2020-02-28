module Fantomas.Tests.GResearchArrayOrListTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = ({ config with
                    AlignBrackets = true
                    SpaceBeforeColon = true
                    SpaceBeforeSemicolon = true })

[<Test>]
let ``array values``() =
    formatSourceString false """
let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]
    """ config
    |> prepend newline
    |> should equal """
let arr =
    [|
        (1, 1, 1)
        (1, 2, 2)
        (1, 3, 3)
        (2, 1, 2)
        (2, 2, 4)
        (2, 3, 6)
        (3, 1, 3)
        (3, 2, 6)
        (3, 3, 9)
    |]
"""

[<Test>]
let ``list values``() =
    formatSourceString false """
let arr = [(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)]
    """ config
    |> prepend newline
    |> should equal """
let arr =
    [
        (1, 1, 1)
        (1, 2, 2)
        (1, 3, 3)
        (2, 1, 2)
        (2, 2, 4)
        (2, 3, 6)
        (3, 1, 3)
        (3, 2, 6)
        (3, 3, 9)
    ]
"""

[<Test>]
let ``short list remains on one line`` () =
    formatSourceString false """let defines = ["FOO";"BAR"]""" config
    |> prepend newline
    |> should equal """
let defines = [ "FOO" ; "BAR" ]
"""