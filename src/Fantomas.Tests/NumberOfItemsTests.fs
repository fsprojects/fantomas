module Fantomas.Tests.NumberOfItemsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

// ****************
// Lists and Arrays
// ****************

[<Test>]
let ``number of items sized lists are formatted properly`` () =
    formatSourceString false """
let xs = [ a; b; c ]
let ys = [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]
f xs [ x; y; z ]
List.map (fun x -> x * x) [1;2;]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems }
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

List.map (fun x -> x * x)
    [ 1
      2 ]
"""

[<Test>]
let ``number of items sized arrays are formatted properly`` () =
    formatSourceString false """
let xs = [| a; b; c |]
let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]
f xs [| x; y; z |]
List.map (fun x -> x * x) [|1;2;|]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems }
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
    [| 1
       2 |]
"""

[<Test>]
let ``number of items sized Elmish lists are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems }
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
let ``number of items sized lists with block brackets on same column are formatted properly`` () =
    formatSourceString false """
let xs = [ a; b; c ]
let ys = [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]
f xs [ x; y; z ]
List.map (fun x -> x * x) [1;2;]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
let xs =
    [
        a
        b
        c
    ]

let ys =
    [
        AReallyLongExpressionThatIsMuchLongerThan50Characters
    ]

f
    xs
    [
        x
        y
        z
    ]

List.map (fun x -> x * x)
    [
        1
        2
    ]
"""

[<Test>]
let ``number of items sized arrays with block brackets on same column are formatted properly`` () =
    formatSourceString false """
let xs = [| a; b; c |]
let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]
f xs [| x; y; z |]
List.map (fun x -> x * x) [|1;2;|]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
let xs =
    [|
        a
        b
        c
    |]

let ys =
    [|
        AReallyLongExpressionThatIsMuchLongerThan50Characters
    |]

f
    xs
    [|
        x
        y
        z
    |]

List.map (fun x -> x * x)
    [|
        1
        2
    |]
"""

[<Test>]
let ``number of items sized Elmish lists with block brackets on same column are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
f [
    a
    b
    c
  ] [
    x
    y
    z
]

g [
    longValueThatIsALotOfCharactersSoooooLong
  ] [
    longValueThatIsALotOfCharactersSoooooLong
]

h [
    longValueThatIsALotOfCharactersSoooooLong
    longValueThatIsALotOfCharactersSoooooLong
  ] [
    longValueThatIsALotOfCharactersSoooooLong
]
"""

[<Test>]
let ``number of items sized Elmish lists with single argument web mode are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ]
g [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              SingleArgumentWebMode = true }
    |> prepend newline
    |> should equal """
f [
    a
    b
    c
]

g [
    longValueThatIsALotOfCharactersSoooooLong
]

h [
    longValueThatIsALotOfCharactersSoooooLong
    longValueThatIsALotOfCharactersSoooooLong
]
"""

[<Test>]
let ``number of items sized Elmish lists with single argument web mode and multiline block brackets on same column are formatted properly`` () =
    formatSourceString false """
f [ a; b; c ]
g [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              SingleArgumentWebMode = true
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
f [
    a
    b
    c
]

g [
    longValueThatIsALotOfCharactersSoooooLong
]

h [
    longValueThatIsALotOfCharactersSoooooLong
    longValueThatIsALotOfCharactersSoooooLong
]
"""

[<Test>]
let ``long expressions with number of items set to 3 will get split due to max line length`` () =
    formatSourceString false """
[ longValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
              ArrayOrListMultilineFormatter = NumberOfItems
              MaxArrayOrListNumberOfItems = 3 }
    |> prepend newline
    |> should equal """
[ longValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLong
  longValueThatIsALotOfCharactersSoooooLong ]
"""

[<Test>]
let ``character width with explicit width sized lists are formatted properly`` () =
    formatSourceString false """
let x = [ a; b; c ]
let y = [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
let z = [ longValueThatIsALotOfCharactersSoooooLong; 100; 123 ]
    """ { config with MaxArrayOrListWidth = 70 }
    |> prepend newline
    |> should equal """
let x = [ a; b; c ]

let y =
    [ longValueThatIsALotOfCharactersSoooooLong
      longValueThatIsALotOfCharactersSoooooLong ]

let z =
    [ longValueThatIsALotOfCharactersSoooooLong; 100; 123 ]
"""

// *******
// Records
// *******

[<Test>]
let ``number of items sized record definitions are formatted properly`` () =
    formatSourceString false """
type R = { a: int; b: string; c: float option }
type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }
    """
        { config with
              RecordMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should equal """
type R =
    { a: int
      b: string
      c: float option }

type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }
"""

[<Test>]
let ``number of items sized record definitions with multiline block brackets on same column are formatted properly`` () =
    formatSourceString false """
type R = { a: int; b: string; c: float option }
type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }
    """
        { config with
              RecordMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
type R =
    {
        a: int
        b: string
        c: float option
    }

type S = { AReallyLongExpressionThatIsMuchLongerThan50Characters: int }
"""

[<Test>]
let ``number of items sized record expressions are formatted properly`` () =
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
              RecordMultilineFormatter = NumberOfItems }
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
let ``number of items sized record expressions with multiline block brackets on same column are formatted properly`` () =
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
              RecordMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
let r =
    {
        a = x
        b = y
        z = c
    }

let s =
    { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

let r' =
    { r with
        a = x
        b = y
        z = c
    }

let s' =
    { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r
    {
        a = x
        b = y
        z = c
    }

g s { AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }

f
    r'
    { r with
        a = x
        b = y
        z = c
    }

g s' { s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 }
"""

[<Test>]
let ``number of items sized anonymous record expressions are formatted properly`` () =
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
              RecordMultilineFormatter = NumberOfItems }
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

[<Test>]
let ``number of items sized anonymous record expressions with multiline block brackets on same column are formatted properly`` () =
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
              RecordMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
let r =
    {|
        a = x
        b = y
        z = c
    |}

let s =
    {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

let r' =
    {| r with
        a = x
        b = y
        z = c
    |}

let s' =
    {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f
    r
    {|
        a = x
        b = y
        z = c
    |}

g s {| AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}

f
    r'
    {| r with
        a = x
        b = y
        z = c
    |}

g s' {| s with AReallyLongExpressionThatIsMuchLongerThan50Characters = 1 |}
"""

[<Test>]
let ``number of items sized anonymous record types are formatted properly`` () =
    formatSourceString false """
let f (x: {| x: int; y: obj |}) = x
let g (x: {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x
type A = {| x: int; y: obj |}
type B = {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}
"""
        { config with
              RecordMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should equal """
let f (x: {| x: int
             y: obj |}) =
    x

let g (x: {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x

type A =
    {| x: int
       y: obj |}

type B = {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}
"""

// FIXME: Change with https://github.com/fsprojects/fantomas/issues/1167
[<Test>]
let ``number of items sized anonymous record types with multiline block brackets on same column are formatted properly`` () =
    formatSourceString false """
let f (x: {| x: int; y: obj |}) = x
let g (x: {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x
type A = {| x: int; y: obj |}
type B = {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}
"""
        { config with
              RecordMultilineFormatter = NumberOfItems
              MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should equal """
let f (x: {| x: int
             y: obj |}) =
    x

let g (x: {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x

type A =
    {| x: int
       y: obj |}

type B = {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}
"""

