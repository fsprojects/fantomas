module Fantomas.Tests.LogicalSize

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

[<Test>]
let ``logically sized lists are formatted properly`` () =
    formatSourceString false """
let xs = [ a; b; c ]
let ys = [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]
f xs [ x; y; z ]
    """
        { config with
              ArrayOrListMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
let xs =
    [ a
      b
      c ]

let ys =
    [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]

f
    xs
    [ x
      y
      z ]
"""

[<Test>]
let ``logically sized arrays are formatted properly`` () =
    formatSourceString false """
let xs = [| a; b; c |]
let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]
f xs [| x; y; z |]
List.map (fun x -> x * x) [1;2;]
    """
        { config with
              ArrayOrListMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
let xs =
    [| a
       b
       c |]

let ys =
    [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]

f
    xs
    [| x
       y
       z |]

List.map (fun x -> x * x)
    [ 1
      2 ]
"""

[<Test>]
let ``logically sized Elmish lists are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
f [ a
    b
    c ] [
    x
    y
    z
]

g [ longValueThatIsALotOfCharactersSoooooLong ] [
    longValueThatIsALotOfCharactersSoooooLong
]

h [ longValueThatIsALotOfCharactersSoooooLong
    longValueThatIsALotOfCharactersSoooooLong ] [
    longValueThatIsALotOfCharactersSoooooLong
]
"""

[<Test>]
let ``character width sized Elmish lists are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ x ]
    """ config
    |> prepend newline
    |> should equal """
f [ a; b; c ] [ x; y; z ]

g [ longValueThatIsALotOfCharactersSoooooLong
    longValueThatIsALotOfCharactersSoooooLong ] [
    x
]
"""

[<Test>]
let ``logically sized record definitions are formatted properly`` () =
    formatSourceString false """
type R = { a: int; b: string; c: float option }
type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }
let f (x: {| a: int; b: string |}) = x.a
let f (x: {| AReallyLongExpressionThatIsMuchLongerThan50Characters: int |}) = x
    """
        { config with
              RecordMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
type R =
    { a: int
      b: string
      c: float option }

type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }

let f (x: {| a: int
             b: string |}) =
    x.a

let f (x: {| AReallyLongExpressionThatIsMuchLongerThan50Characters: int |}) = x
"""

[<Test>]
let ``logically sized record expressions are formatted properly`` () =
    formatSourceString false """
let r = { a = x; b = y; z = c }
let s = { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

let r' = { r with a = x; b = y; z = c }
let s' = { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f r { a = x; b = y; z = c }
g s { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f r' { r with a = x; b = y; z = c }
g s' { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }
    """
        { config with
              RecordMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
let r =
    { a = x
      b = y
      z = c }

let s =
    { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

let r' =
    { r with
          a = x
          b = y
          z = c }

let s' =
    { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r
    { a = x
      b = y
      z = c }

g s { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r'
    { r with
          a = x
          b = y
          z = c }

g s' { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }
"""

[<Test>]
let ``logically sized anonymous record expressions are formatted properly`` () =
    formatSourceString false """
let r = {| a = x; b = y; z = c |}
let s = {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

let r' = {| r with a = x; b = y; z = c |}
let s' = {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f r {| a = x; b = y; z = c |}
g s {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f r' {| r with a = x; b = y; z = c |}
g s' {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}
    """
        { config with
              RecordMultilineFormatter = LogicalSize }
    |> prepend newline
    |> should equal """
let r =
    {| a = x
       b = y
       z = c |}

let s =
    {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

let r' =
    {| r with
           a = x
           b = y
           z = c |}

let s' =
    {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f
    r
    {| a = x
       b = y
       z = c |}

g s {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f
    r'
    {| r with
           a = x
           b = y
           z = c |}

g s' {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}
"""
