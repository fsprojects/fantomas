module Fantomas.Core.Tests.AlignedMultilineBracketStyleArrayOrListTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Aligned
        SpaceBeforeColon = true
        SpaceBeforeSemicolon = true }

[<Test>]
let ``array values`` () =
    formatSourceString
        false
        """
let arr = [|(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)|]
    """
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``list values`` () =
    formatSourceString
        false
        """
let arr = [(1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
  (3, 2, 6); (3, 3, 9)]
    """
        config
    |> prepend newline
    |> should
        equal
        """
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
    |> should
        equal
        """
let defines = [ "FOO" ; "BAR" ]
"""

[<Test>]
let ``array patterns`` () =
    formatSourceString
        false
        """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1; var2 |] -> sqrt (var1*var1 + var2*var2)
    | [| var1; var2; var3 |] -> sqrt (var1*var1 + var2*var2 + var3*var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)"""
        config
    |> prepend newline
    |> should
        equal
        """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1 ; var2 |] -> sqrt (var1 * var1 + var2 * var2)
    | [| var1 ; var2 ; var3 |] -> sqrt (var1 * var1 + var2 * var2 + var3 * var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)
"""

[<Test>]
let ``array comprehensions`` () =
    formatSourceString
        false
        """
let a1 = [| 0 .. 99 |]
let a2 = [| for n in 1 .. 100 do if isPrime n then yield n |]"""
        { config with MaxIfThenShortWidth = 25 }
    |> prepend newline
    |> should
        equal
        """
let a1 = [| 0..99 |]

let a2 =
    [|
        for n in 1..100 do
            if isPrime n then yield n
    |]
"""

[<Test>]
let ``line comment after opening bracket list`` () =
    formatSourceString
        false
        """let a = [ // some line comment
    (1,2,3); (4,5,6); (7,8,9) ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    [ // some line comment
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    ]
"""

[<Test>]
let ``line comment after opening bracket in short list`` () =
    formatSourceString
        false
        """let a = [ // some line comment
    a;b ]
let bb = b
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    [ // some line comment
        a
        b
    ]

let bb = b
"""

[<Test>]
let ``line comment after opening bracket array`` () =
    formatSourceString
        false
        """let a = [| // some line comment
    (1,2,3); (4,5,6); (7,8,9) |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    [| // some line comment
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    |]
"""

[<Test>]
let ``line comment before closing bracket list`` () =
    formatSourceString
        false
        """let a = [
    (1,2,3); (4,5,6); (7,8,9)
    // some line comment
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let a = [|
    (1,2,3); (4,5,6); (7,8,9)
    // some line comment
    |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    [|
        (1, 2, 3)
        (4, 5, 6)
        (7, 8, 9)
    // some line comment
    |]
"""

[<Test>]
let ``multiline function application inside array`` () =
    formatSourceString
        false
        """
[| Abc(
    deffffffffffffffffffffff,
    ghiiiiiiiiiiiiiiiiiiiiiii,
    jklllllllllllllllllllllll,
    qweeeeeeeeeeeeeeeeeeeeeee,
    uioooooooooooooooooooooooo
  ) |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[|
    Abc(
        deffffffffffffffffffffff,
        ghiiiiiiiiiiiiiiiiiiiiiii,
        jklllllllllllllllllllllll,
        qweeeeeeeeeeeeeeeeeeeeeee,
        uioooooooooooooooooooooooo
    )
|]
"""

[<Test>]
let ``multiline function application inside list`` () =
    formatSourceString
        false
        """
[ myFunction(
    deffffffffffffffffffffff,
    ghiiiiiiiiiiiiiiiiiiiiiii,
    jklllllllllllllllllllllll,
    qweeeeeeeeeeeeeeeeeeeeeee,
    uioooooooooooooooooooooooo)
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[
    myFunction (
        deffffffffffffffffffffff,
        ghiiiiiiiiiiiiiiiiiiiiiii,
        jklllllllllllllllllllllll,
        qweeeeeeeeeeeeeeeeeeeeeee,
        uioooooooooooooooooooooooo
    )
]
"""

[<Test>]
let ``list with only lambda expressions should be multiline, 1405`` () =
    formatSourceString
        false
        """
let foo =
    [
        fun () -> 1
        fun () -> 2
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo =
    [
        fun () -> 1
        fun () -> 2
    ]
"""

[<Test>]
let ``array with only lambda expressions should be multiline`` () =
    formatSourceString
        false
        """
let foo =
    [|
        fun () -> 1
        fun () -> 2
    |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo =
    [|
        fun () -> 1
        fun () -> 2
    |]
"""

[<Test>]
let ``comments before closing bracket`` () =
    formatSourceString
        false
        """
let fns =
    [ { x = "long enough to not go to one line"
        y = 5 }
 //      { name = fn "String" "endsWith" 0
 //        deprecated = NotDeprecated }
 // I think the space at the start of the lines above matter
     ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fns =
    [
        {
            x = "long enough to not go to one line"
            y = 5
        }
    //      { name = fn "String" "endsWith" 0
    //        deprecated = NotDeprecated }
    // I think the space at the start of the lines above matter
    ]
"""

[<Test>]
let ``comments before closing bracket, array`` () =
    formatSourceString
        false
        """
let fns =
    [| { x = "long enough to not go to one line"
         y = 5 }
 //      { name = fn "String" "endsWith" 0
 //        deprecated = NotDeprecated }
 // I think the space at the start of the lines above matter
    |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fns =
    [|
        {
            x = "long enough to not go to one line"
            y = 5
        }
    //      { name = fn "String" "endsWith" 0
    //        deprecated = NotDeprecated }
    // I think the space at the start of the lines above matter
    |]
"""

[<Test>]
let ``long list in for loop, 1650`` () =
    formatSourceString
        false
        """
module Foo =

    let foo () =
        let bar =
            seq {
                for i in ["hello1" ; "hello1" ; "hello1" ; "hello1" ; "hello1"] do
                    yield i, seq {
                        yield "hi"
                        yield "bye"
                    }
            }
        ()
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            MultiLineLambdaClosingNewline = true
            MaxArrayOrListWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let foo () =
        let bar =
            seq {
                for i in
                    [
                        "hello1"
                        "hello1"
                        "hello1"
                        "hello1"
                        "hello1"
                    ] do
                    yield
                        i,
                        seq {
                            yield "hi"
                            yield "bye"
                        }
            }

        ()
"""
