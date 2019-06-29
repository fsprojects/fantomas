module Fantomas.Tests.LetBindingTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``let in should be preserved``() =
    formatSourceString false "let x = 1 in ()" config
    |> should equal """let x = 1 in ()
"""

[<Test>]
let ``let in should be preserved with PreserveEOL option``() =
    formatSourceString false "let x = 1 in ()" { config with PreserveEndOfLine = true }
    |> should equal """let x = 1 in ()
"""

[<Test>]
let ``multiple let in lines, should remove in`` () =
    let codeSnippet = """
let f () = 
  let x = 1 in   // the "in" keyword is available in F#
    let y = 2 in 
      x + y
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1 // the "in" keyword is available in F#
    let y = 2
    x + y
"""

[<Test>]
let ``multiple let in lines, should remove in, block comment`` () =
    let codeSnippet = """
let f () = 
  let x = 1 in   (* the "in" keyword is available in F# *)
    let y = 2 in 
      x + y
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1 (* the "in" keyword is available in F# *)
    let y = 2
    x + y
"""

[<Test>]
let ``multiline let in, should remove in`` () =
    let codeSnippet = """
let f () =
  let x = 1 in if true 
               then x
               else x
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1
    if true then x
    else x
"""

[<Test>]
let ``multiline let in, should remove in 2`` () =
    let codeSnippet = """
let f () =
  let x = 1 in (while true do ()
                x)
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1
    (while true do
        ()
     x)
"""

[<Test>]
let ``DotGet on newline should be indented far enough`` () =
    formatSourceString false """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""  config
    |> prepend newline
    |> should equal """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""

[<Test>]
let ``DotGet on newline after empty string should be indented far enough`` () =
    formatSourceString false """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""  config
    |> prepend newline
    |> should equal """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""