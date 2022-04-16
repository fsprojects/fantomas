module Fantomas.Tests.Ragnarok.FunctionApplicationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
        MultilineBlockBracketsOnSameColumn = true
        Ragnarok = true }


// testList "A test group" [ 
//     test "one test" { Expect.equal (2 + 2) 4 "2+2" }

//     test "another test that fails" { Expect.equal (3 + 3) 5 "3+3" }

//     testAsync "this is an async test" {
//         let! x = async { return 4 }
//         Expect.equal x (2 + 2) "2+2"
//     }

//     testTask "this is a task test" {
//         let! n = Task.FromResult 2
//         Expect.equal n 2 "n=2"
//     } 
// ]


[<Test>]
[<Category("FunctionApplicationTests")>]
let ``functionapplication elements in the last list empty fit on one line`` () =
    formatSourceString
        false
        """
functionName a b [   ]
    """
            config
        |> prepend newline
        |> should
            equal
            """
functionName a b []
"""

[<Test>]
[<Category("FunctionApplicationTests")>]
let ``functionapplication elements in the last list argument fits on one line`` () =
    formatSourceString
        false
        """
functionName a b [  c; d  ]
    """
            config
        |> prepend newline
        |> should
            equal
            """
functionName a b [ c; d ]
"""


[<Test>]
[<Category("FunctionApplicationTests")>]
let ``functionapplication elements in the last list containing trivia`` () =
    formatSourceString
        false
        """
functionName a b [ // comment
                                c; d ]
    """
            config
        |> prepend newline
        |> should
            equal
            """
functionName a b [ // comment
    c
    d
]
"""

[<Test>]
[<Category("FunctionApplicationTests")>]
let ``functionapplication testList testList single item`` () =
    formatSourceString
        false
        """
testList "A test group" [
    testAsync "this is an async test" {
        let! x = async { return 4 }
        Expect.equal x (2 + 2) "2+2"
    }
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
testList "A test group" [
    testAsync "this is an async test" {
        let! x = async { return 4 }
        Expect.equal x (2 + 2) "2+2"
    }
]
"""


