module Fantomas.Tests.LogicalSize

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

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
