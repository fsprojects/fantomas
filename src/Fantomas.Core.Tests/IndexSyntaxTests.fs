module Fantomas.Core.Tests.IndexSyntaxTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``don't convert index syntax without dot to application`` () =
    formatSourceString
        """
expr1[expr2]
"""
        config
    |> prepend newline
    |> should
        equal
        """
expr1[expr2]
"""

[<Test>]
let ``slicing examples`` () =
    formatSourceString
        """
let arr = [| 1;2;3 |]
arr[0] <- 2
arr[0]
arr[0..1]
arr[..1]
arr[0..]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let arr = [| 1; 2; 3 |]
arr[0] <- 2
arr[0]
arr[0..1]
arr[..1]
arr[0..]
"""

[<Test>]
let ``higher-dimensional arrays`` () =
    formatSourceString
        """
let arr = Array4D.create 3 4 5 6 0
arr[0,2,3,4] <- 2
arr[0,2,3,4]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let arr = Array4D.create 3 4 5 6 0
arr[0, 2, 3, 4] <- 2
arr[0, 2, 3, 4]
"""

[<Test>]
let ``index syntax without dot on array of arrays, 2151`` () =
    formatSourceString
        """
let a = Array.create 10 -1
let b = Array.create 10 a

printfn "%d -> %d" a[0] (b[0][0])
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = Array.create 10 -1
let b = Array.create 10 a

printfn "%d -> %d" a[0] (b[0][0])
"""

[<Test>]
let ``only add spaces when expressions are atomic`` () =
    formatSourceString
        """
let a = [ 2 .. 7 ] // integers
let b = [ one .. two ] // identifiers
let c = [ .. 9 ] // also when there is only one expression
let d = [ 0.7 .. 9.2 ] // doubles
let e = [ 2L .. number / 2L ] // complex expression
let f = [| A.B .. C.D |] // identifiers with dots
let g = [ .. (39 - 3) ] // complex expression
let h = [| 1 .. MyModule.SomeConst |] // not all expressions are atomic
for x in 1 .. 2 do
    printfn " x = %d" x
let s = seq { 0..10..100 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = [ 2..7 ] // integers
let b = [ one..two ] // identifiers
let c = [ ..9 ] // also when there is only one expression
let d = [ 0.7 .. 9.2 ] // doubles
let e = [ 2L .. number / 2L ] // complex expression
let f = [| A.B .. C.D |] // identifiers with dots
let g = [ .. (39 - 3) ] // complex expression
let h = [| 1 .. MyModule.SomeConst |] // not all expressions are atomic

for x in 1..2 do
    printfn " x = %d" x

let s = seq { 0..10..100 }
"""

[<Test>]
let ``index syntax on raw list, 1929`` () =
    formatSourceString
        """
let y = [ 0; 2; 4 ][ 1 ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y = [ 0; 2; 4 ][1]
"""

[<Test>]
let ``index syntax on dotget, 1985`` () =
    formatSourceString
        """
let segment = System.Uri(ctx.Request.Path.Value).Segments[1]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let segment = System.Uri(ctx.Request.Path.Value).Segments[1]
"""

[<Test>]
let ``ident and negative number should keep space, 2071`` () =
    formatSourceString
        """
  do
    for i in [ maxIndex .. -1 .. startIndex ] do
      stack.Push i
"""
        config
    |> prepend newline
    |> should
        equal
        """
do
    for i in [ maxIndex .. -1 .. startIndex ] do
        stack.Push i
"""

[<Test>]
let ``float range with trailing zero omitted, 2171`` () =
    formatSourceString
        """
let a = [1. .. 0.1 .. 2.]
let b = [1.0 .. 2. .. 10.]
let c = [1.0 .. 2.0 .. 10.0]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = [ 1. .. 0.1 .. 2. ]
let b = [ 1.0 .. 2. .. 10. ]
let c = [ 1.0..2.0..10.0 ]
"""

[<Test>]
let ``indexed item invocation, 2106`` () =
    formatSourceString
        """
array1[0]()
"""
        config
    |> prepend newline
    |> should
        equal
        """
array1[0]()
"""

[<Test>]
let ``nested indexed item`` () =
    formatSourceString
        """
let x = array1[0][0]
let y = callData["key"]["subKey"]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = array1[0][0]
let y = callData["key"]["subKey"]
"""

[<Test>]
let ``triple nested indexed item`` () =
    formatSourceString
        """
let meh = myList[0][1][2]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let meh = myList[0][1][2]
"""

[<Test>]
let ``prefixed index syntax, 2494`` () =
    formatSourceString
        """
b[c] <- d
a.b[c] <- d
"""
        config
    |> prepend newline
    |> should
        equal
        """
b[c] <- d
a.b[c] <- d
"""

[<Test>]
let ``comment is removed when using array index access syntax, 2611`` () =
    formatSourceString
        """
open System.Collections.Generic

let inventory = Dictionary<string, int>()

inventory.Add("Apples", 1)
inventory.Add("Oranges", 2)
inventory.Add("Bananas", 3)

inventory["Oranges"] // raises an exception if not found
inventory.["Apples"] // raises an exception if not found
nestedInventory["Oranges"][23] // raises an exception if not found
"""
        config
    |> prepend newline
    |> should
        equal
        """
open System.Collections.Generic

let inventory = Dictionary<string, int>()

inventory.Add("Apples", 1)
inventory.Add("Oranges", 2)
inventory.Add("Bananas", 3)

inventory["Oranges"] // raises an exception if not found
inventory.["Apples"] // raises an exception if not found
nestedInventory["Oranges"][23] // raises an exception if not found
"""
