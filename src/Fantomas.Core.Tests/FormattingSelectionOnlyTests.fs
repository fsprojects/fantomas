module Fantomas.Core.Tests.FormattingSelectionOnlyTests

open FSharp.Compiler.Text
open Fantomas.Core
open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let private config = FormatConfig.FormatConfig.Default

let private formatSelectionOnly isFsiFile selection (source: string) config =
    let formattedSelection, _ =
        CodeFormatter.FormatSelectionAsync(isFsiFile, source, selection, config)
        |> Async.RunSynchronously

    formattedSelection.Replace("\r\n", "\n")

let private formatSelectionAndAssertRange isFsiFile selection source config =
    let _, resultRange =
        CodeFormatter.FormatSelectionAsync(isFsiFile, source, selection, config)
        |> Async.RunSynchronously

    (resultRange.StartLine, resultRange.StartColumn, resultRange.EndLine, resultRange.EndColumn)
    == (selection.StartLine, selection.StartColumn, selection.EndLine, selection.EndColumn)

let private mkSelection (startLine, startColumn) (endLine, endColumn) =
    let startPos = Position.mkPos startLine startColumn
    let endPos = Position.mkPos endLine endColumn
    Range.mkRange "selection" startPos endPos

[<Test>]
let ``SynModuleDecl with exact selection`` () =
    formatSelectionOnly
        false
        (mkSelection (6, 0) (6, 12))
        """
module A

let x = 1

let y =    2
"""
        config
    |> should equal "let y = 2"

[<Test>]
let ``SynExpr with exact selection`` () =
    formatSelectionOnly
        false
        (mkSelection (4, 13) (4, 16))
        """
module A

let two =    1+1
"""
        config
    |> should equal "1 + 1"

[<Test>]
let ``SynExpr with larger selection`` () =
    formatSelectionOnly
        false
        (mkSelection (4, 0) (10, 0))
        """
let x =
    printfn "start"

    try
        something ()
    with
    | ex ->
        printfn "caught"

    ()
"""
        config
    |> prepend newline
    |> should
        equal
        "
try
    something ()
with ex ->
    printfn \"caught\""

/// The point of this test is to format the function application as multiline.
/// Because that is what would happen if the entire file was formatted.
[<Test>]
let ``indented SynExpr with exact selection`` () =
    formatSelectionOnly
        false
        (mkSelection (10, 25) (10, 82))
        """
module A

let v =  functionApplication argumentOne argumentTwo argumentThree

module B =
    module C =
        module D =
            module E =
                let y =  functionApplication argumentOne argumentTwo argumentThree
"""
        { config with MaxLineLength = 70 }
    |> prepend newline
    |> should
        equal
        """
functionApplication
    argumentOne
    argumentTwo
    argumentThree"""

[<Test>]
let ``format block comment inside selection`` () =
    formatSelectionOnly
        false
        (mkSelection (2, 8) (2, 82))
        """
let v = functionApplication argumentOne   (* FOOBAR *)   argumentTwo argumentThree
"""
        config
    |> prepend newline
    |> should
        equal
        """
functionApplication argumentOne (* FOOBAR *) argumentTwo argumentThree"""

[<Test>]
let ``format top level signature value`` () =
    formatSelectionOnly
        true
        (mkSelection (2, 0) (2, 20))
        """
val meh     :    int
val otherThing: string -> string
"""
        config
    |> prepend newline
    |> should
        equal
        """
val meh: int"""

[<Test>]
let ``format type definition`` () =
    formatSelectionOnly
        false
        (mkSelection (2, 0) (6, 1))
        """
type Meh = {
    X : int
    Y: string
    Z: DateTime
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh = { X: int; Y: string; Z: DateTime }"""

[<Test>]
let ``format type definition in signature file`` () =
    formatSelectionOnly
        true
        (mkSelection (4, 0) (8, 1))
        """
namespace A.B.C

type Meh = {
    X : int
    Y: string
    Z: DateTime
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh = { X: int; Y: string; Z: DateTime }"""

[<Test>]
let ``format addition`` () =
    formatSelectionAndAssertRange
        false
        (mkSelection (3, 8) (3, 17))
        """
let x = 9
let y = a   +   x
let z = ""
"""
        config

[<Test>]
let ``format second binding of recursive binding`` () =
    formatSelectionOnly
        false
        (mkSelection (6, 0) (6, 23))
        """
module A

let rec x y z = 1

and a    b  c    =    2
"""
        config
    |> should equal "and a b c = 2"
