module Fantomas.Tests.KeepIfThenInSameLineTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = { config with
                KeepIfThenInSameLine = true
                MaxLineLength = 80
                MaxIfThenElseShortWidth = 0 }

[<Test>]
let ``if only, 825`` () =
    formatSourceString false """type TransferAmount(valueToSend: decimal, balanceAtTheMomentOfSending: decimal) =
    do
        if balanceAtTheMomentOfSending < valueToSend then
            invalidArg "balanceAtTheMomentOfSending"
                "some very very long error message"
        if valueToSend <= 0m then
            invalidArg "valueToSend" "Amount has to be above zero"
"""  { config with MaxLineLength = 100 }
    |> prepend newline
    |> should equal """
type TransferAmount(valueToSend: decimal, balanceAtTheMomentOfSending: decimal) =
    do
        if balanceAtTheMomentOfSending < valueToSend then
            invalidArg "balanceAtTheMomentOfSending" "some very very long error message"
        if valueToSend <= 0m then
            invalidArg "valueToSend" "Amount has to be above zero"
"""

[<Test>]
let ``if with comments, 825`` () =
    formatSourceString false """type TransferAmount(valueToSend: decimal, balanceAtTheMomentOfSending: decimal) =
    do
        // comment
        if balanceAtTheMomentOfSending < valueToSend then // comment
            invalidArg "balanceAtTheMomentOfSending" // comment
                "some very very long error message" // comment
        if valueToSend <= 0m then // comment
            invalidArg "valueToSend" "Amount has to be above zero" // comment
"""  config
    |> prepend newline
    |> should equal """
type TransferAmount(valueToSend: decimal, balanceAtTheMomentOfSending: decimal) =
    do
        // comment
        if balanceAtTheMomentOfSending < valueToSend then // comment
            invalidArg
                "balanceAtTheMomentOfSending"  // comment
                "some very very long error message" // comment
        if valueToSend <= 0m then // comment
            invalidArg "valueToSend" "Amount has to be above zero" // comment
"""

[<Test>]
let ``if, else if, else, 825`` () =
    formatSourceString false """if foooooooooooooooooooooooooooooooooooooooo then
    a
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    b
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    c
else
    d
"""  config
    |> prepend newline
    |> should equal """
if foooooooooooooooooooooooooooooooooooooooo then
    a
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    b
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    c
else
    d
"""

[<Test>]
let ``if, else if, else, with comments 825`` () =
    formatSourceString false """// comment
if foooooooooooooooooooooooooooooooooooooooo then // comment
    a // comment
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then // comment
    b // comment
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then // comment
    c // comment
else // comment
    d // comment
"""  config
    |> prepend newline
    |> should equal """
// comment
if foooooooooooooooooooooooooooooooooooooooo then // comment
    a // comment
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then // comment
    b // comment
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then // comment
    c // comment
else // comment
    d // comment
"""

[<Test>]
let ``if, else if, else, multiline, 825`` () =
    formatSourceString false """if foooooooooooooooooooooooooooooooooooooooo then
    multi
    line
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    multi
    line
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    multi
    line
else
    multi
    line
"""  config
    |> prepend newline
    |> should equal """
if foooooooooooooooooooooooooooooooooooooooo then
    multi
    line
else if baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    multi
    line
elif caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar then
    multi
    line
else
    multi
    line
"""

[<Test>]
let ``MaxIfThenElseShortWidth not exceeded, 825`` () =
    let c = { config with
               MaxIfThenElseShortWidth = 100 }
               
    formatSourceString false """if foo then
    bar
else if fooo then
    bar
elif foooo then
    bar
else
    bar
"""  c
    |> prepend newline
    |> should equal """
if foo then bar
else if fooo then bar
elif foooo then bar
else bar
"""
