module Fantomas.Tests.MultilineBlockBracketsOnSameColumnArrayOrListTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = ({ config with
                    MultilineBlockBracketsOnSameColumn = true
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

[<Test>]
let ``array patterns``() =
    formatSourceString false """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1; var2 |] -> sqrt (var1*var1 + var2*var2)
    | [| var1; var2; var3 |] -> sqrt (var1*var1 + var2*var2 + var3*var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)""" config
    |> prepend newline
    |> should equal """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1 ; var2 |] -> sqrt (var1 * var1 + var2 * var2)
    | [| var1 ; var2 ; var3 |] -> sqrt (var1 * var1 + var2 * var2 + var3 * var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)
"""

[<Test>]
let ``array comprehensions``() =
    formatSourceString false """
let a1 = [| 0 .. 99 |]
let a2 = [| for n in 1 .. 100 do if isPrime n then yield n |]""" config
    |> prepend newline
    |> should equal """
let a1 = [| 0 .. 99 |]

let a2 =
    [|
        for n in 1 .. 100 do
            if isPrime n then yield n
    |]
"""

[<Test>]
let ``line comment after opening bracket list`` () =
    formatSourceString false """let a = [ // some line comment
    (1,2,3); (4,5,6); (7,8,9) ]
"""  config
    |> prepend newline
    |> should equal """
let a =
    [ // some line comment
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    ]
"""

[<Test>]
let ``line comment after opening bracket in short list`` () =
    formatSourceString false """let a = [ // some line comment
    a;b ]
let bb = b
"""  config
    |> prepend newline
    |> should equal """
let a =
    [ // some line comment
        a
        b
    ]

let bb = b
"""

[<Test>]
let ``line comment after opening bracket array`` () =
    formatSourceString false """let a = [| // some line comment
    (1,2,3); (4,5,6); (7,8,9) |]
"""  config
    |> prepend newline
    |> should equal """
let a =
    [| // some line comment
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    |]
"""

[<Test>]
let ``line comment before closing bracket list`` () =
    formatSourceString false """let a = [
    (1,2,3); (4,5,6); (7,8,9)
    // some line comment
    ]
"""  config
    |> prepend newline
    |> should equal """
let a =
    [
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    // some line comment
    ]
"""

[<Test>]
let ``line comment before closing bracket array`` () =
    formatSourceString false """let a = [|
    (1,2,3); (4,5,6); (7,8,9)
    // some line comment
    |]
"""  config
    |> prepend newline
    |> should equal """
let a =
    [|
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    // some line comment
    |]
"""
