module Fantomas.Tests.LetBindingTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
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
    let x = 1   // the "in" keyword is available in F#
    let y = 2
    x + y
"""