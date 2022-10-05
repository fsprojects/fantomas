module Fantomas.Core.Tests.LineCommentAfterTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``the one million dollar fix`` () =
    formatSourceString
        false
        """
[
    "thing"
    "thing2"
    // no other things
]
"""
        { config with MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
[
    "thing"
    "thing2"
    // no other things
]
"""

[<Test>]
let ``this even works nested, oh my`` () =
    formatSourceString
        false
        """
let v =
    [
        "thing"
        "thing2"
        // no other things
    ]
"""
        { config with MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
let v =
    [
        "thing"
        "thing2"
        // no other things
    ]
"""

[<Test>]
let ``how adding one blank line really complicates things`` () =
    formatSourceString
        false
        """
let v =
    [|
        "thing"
        "thing2"

        // there is a blank line above this comment
    |]
"""
        { config with MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
let v =
    [|
        "thing"
        "thing2"

        // there is a blank line above this comment
    |]
"""

[<Test>]
let ``this really proves the point of the methodology`` () =
    formatSourceString
        false
        """
let v =
    [
        "thing"
        "thing2"

        // there is a blank line above this comment
    ]
"""
        { config with
            MultilineBlockBracketsOnSameColumn = true
            IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let v =
  [
    "thing"
    "thing2"

    // there is a blank line above this comment
  ]
"""

[<Test>]
let ``putting on my clown wig`` () =
    formatSourceString
        false
        """
module Foo =
    [<Test>]
    let bar () =
        foo ()
        foo ()
        // thing

    [<Test>]
    let baz () = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    [<Test>]
    let bar () =
        foo ()
        foo ()
        // thing

    [<Test>]
    let baz () = ()
"""
