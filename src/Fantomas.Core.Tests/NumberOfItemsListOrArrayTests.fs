module Fantomas.Core.Tests.NumberOfItemsListOrArrayTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core.FormatConfig

[<Test>]
let ``number of items sized lists are formatted properly`` () =
    formatSourceString
        false
        """
let xs = [ a; b; c ]
let ys = [ AReallyLongExpressionThatIsMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerThan100Characters ]
f xs [ x; y; z ]
List.map (fun x -> x * x) [1;2;]
    """
        { config with ArrayOrListMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should
        equal
        """
let xs =
    [ a
      b
      c ]

let ys =
    [ AReallyLongExpressionThatIsMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerThan100Characters ]

f
    xs
    [ x
      y
      z ]

List.map
    (fun x -> x * x)
    [ 1
      2 ]
"""

[<Test>]
let ``number of items sized arrays are formatted properly`` () =
    formatSourceString
        false
        """
let xs = [| a; b; c |]
let ys = [| AReallyLongExpressionThatIsMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerThan100Characters |]
f xs [| x; y; z |]
List.map (fun x -> x * x) [|1;2;|]
    """
        { config with ArrayOrListMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should
        equal
        """
let xs =
    [| a
       b
       c |]

let ys =
    [| AReallyLongExpressionThatIsMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerMuchLongerThan100Characters |]

f
    xs
    [| x
       y
       z |]

List.map
    (fun x -> x * x)
    [| 1
       2 |]
"""

[<Test>]
[<Ignore "Revisit">]
let ``number of items sized Elmish lists are formatted properly`` () =
    formatSourceString
        false
        """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with ArrayOrListMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let xs = [ a; b; c ]
let ys = [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]
f xs [ x; y; z ]
List.map (fun x -> x * x) [1;2;]
    """
        { config with
            ArrayOrListMultilineFormatter = NumberOfItems
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
let xs =
    [
        a
        b
        c
    ]

let ys = [ AReallyLongExpressionThatIsMuchLongerThan50Characters ]

f
    xs
    [
        x
        y
        z
    ]

List.map
    (fun x -> x * x)
    [
        1
        2
    ]
"""

[<Test>]
let ``number of items sized arrays with block brackets on same column are formatted properly`` () =
    formatSourceString
        false
        """
let xs = [| a; b; c |]
let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]
f xs [| x; y; z |]
List.map (fun x -> x * x) [|1;2;|]
    """
        { config with
            ArrayOrListMultilineFormatter = NumberOfItems
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
let xs =
    [|
        a
        b
        c
    |]

let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]

f
    xs
    [|
        x
        y
        z
    |]

List.map
    (fun x -> x * x)
    [|
        1
        2
    |]
"""

[<Test>]
[<Ignore "Revisit">]
let ``number of items sized Elmish lists with block brackets on same column are formatted properly`` () =
    formatSourceString
        false
        """
f [ a; b; c ] [ x; y; z ]
g [ longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ] [ longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
            ArrayOrListMultilineFormatter = NumberOfItems
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
f [
    a
    b
    c
  ] [
    x
    y
    z
]

g [ longValueThatIsALotOfCharactersSoooooLong ] [
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
[<Ignore "Revisit">]
let ``number of items sized Elmish lists with single argument web mode are formatted properly`` () =
    formatSourceString
        false
        """
f [ a; b; c ]
g [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with ArrayOrListMultilineFormatter = NumberOfItems }
    |> prepend newline
    |> should
        equal
        """
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
[<Ignore "Revisit">]
let ``number of items sized Elmish lists with single argument web mode and multiline block brackets on same column are formatted properly``
    ()
    =
    formatSourceString
        false
        """
f [ a; b; c ]
g [ longValueThatIsALotOfCharactersSoooooLong ]
h [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
            ArrayOrListMultilineFormatter = NumberOfItems
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
[ longValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
    """
        { config with
            ArrayOrListMultilineFormatter = NumberOfItems
            MaxArrayOrListNumberOfItems = 3 }
    |> prepend newline
    |> should
        equal
        """
[ longValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLongAndlongValueThatIsALotOfCharactersSoooooLong
  longValueThatIsALotOfCharactersSoooooLong ]
"""

[<Test>]
let ``character width with explicit width sized lists are formatted properly`` () =
    formatSourceString
        false
        """
let x = [ a; b; c ]
let y = [ longValueThatIsALotOfCharactersSoooooLong; longValueThatIsALotOfCharactersSoooooLong ]
let z = [ longValueThatIsALotOfCharactersSoooooLong; 100; 123 ]
    """
        { config with MaxArrayOrListWidth = 70 }
    |> prepend newline
    |> should
        equal
        """
let x = [ a; b; c ]

let y =
    [ longValueThatIsALotOfCharactersSoooooLong
      longValueThatIsALotOfCharactersSoooooLong ]

let z = [ longValueThatIsALotOfCharactersSoooooLong; 100; 123 ]
"""
