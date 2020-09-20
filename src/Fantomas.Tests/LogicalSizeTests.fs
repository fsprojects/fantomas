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
