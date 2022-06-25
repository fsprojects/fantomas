module Fantomas.Core.Tests.FormattingSelectionOnlyTests

open FSharp.Compiler.Text
open Fantomas.Core
open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let private config = FormatConfig.FormatConfig.Default

let private formatSelectionOnly isFsiFile selection (source: string) config =
    let result =
        CodeFormatter.FormatSelectionAsync(isFsiFile, source, selection, config)
        |> Async.RunSynchronously

    match result with
    | Some (formattedSelection, _) -> formattedSelection.Replace("\r\n", "\n")
    | None ->
        Assert.Fail("CodeFormatter.FormatSelectionAsync did not yield a result")
        System.String.Empty

let private formatSelectionAndAssertRange isFsiFile selection source config =
    let result =
        CodeFormatter.FormatSelectionAsync(isFsiFile, source, selection, config)
        |> Async.RunSynchronously

    match result with
    | Some (_, range) ->
        (range.StartLine, range.StartColumn, range.EndLine, range.EndColumn)
        == (selection.StartLine, selection.StartColumn, selection.EndLine, selection.EndColumn)
    | None -> Assert.Fail("CodeFormatter.FormatSelectionAsync did not yield a result")

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

// 10	25	10	82
//[<Test>]
//let ``should format a part of a line correctly`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 8 3 10)
//        """
//let x = 2 + 3
//let y = 1+2
//let z = x + y"""
//        config
//    |> should equal """1 + 2"""
//
//[<Test>]
//let ``should format a whole line correctly and preserve indentation`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 0 3 36)
//        """
//    let base1 = d1 :> Base1
//    let derived1 = base1 :?> Derived1"""
//        config
//    |> should equal """    let derived1 = base1 :?> Derived1"""
//
//[<Test>]
//let ``should format a few lines correctly and preserve indentation`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 4 5 51)
//        """
//let rangeTest testValue mid size =
//    match testValue with
//    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
//    | _ -> printfn "The test value is out of range."
//
//let (var1, var2) as tuple1 = (1, 2)"""
//        config
//    |> append newline
//    |> should
//        equal
//        """match testValue with
//    | var1 when var1 >= mid - size / 2 && var1 <= mid + size / 2 -> printfn "The test value is in range."
//    | _ -> printfn "The test value is out of range."
//"""
//
//[<Test>]
//let ``should format a top-level let correctly`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 0 3 10)
//        """
//let x = 2 + 3
//let y = 1+2
//let z = x + y"""
//        config
//    |> should equal """let y = 1 + 2"""
//
//[<Test>]
//let ``should skip whitespace at the beginning of lines`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 3 3 27)
//        """
//type Product' (backlogItemId) =
//    let mutable ordering = 0
//    let mutable version = 0
//    let backlogItems = []"""
//        config
//    |> should equal """ let mutable ordering = 0"""
//
//[<Test>]
//let ``should parse a complete expression correctly`` () =
//    formatSelectionOnly
//        false
//        (makeRange 4 0 5 35)
//        """
//open Fantomas.CodeFormatter
//
//let config = { FormatConfig.Default with
//                IndentSpaceNum = 2 }
//
//let source = "
//    let Multiple9x9 () =
//      for i in 1 .. 9 do
//        printf \"\\n\";
//        for j in 1 .. 9 do
//          let k = i * j in
//          printf \"%d x %d = %2d \" i j k;
//          done;
//      done;;
//    Multiple9x9 ();;"
//"""
//        { config with
//            MaxValueBindingWidth = 120
//            MaxRecordWidth = 50 }
//    |> should equal """let config = { FormatConfig.Default with IndentSpaceNum = 2 }"""
//
//[<Test>]
//let ``should format the selected pipeline correctly`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 4 7 18)
//        """
//let r =
//    [ "abc"
//      "a"
//      "b"
//      "" ]
//    |> List.map id"""
//        config
//    |> should equal """[ "abc"; "a"; "b"; "" ] |> List.map id"""
//
//[<Test>]
//let ``should preserve line breaks before and after selection`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 0 4 25)
//        """
//assert (3 > 2)
//
//let result = lazy (x + 10)
//
//do printfn "Hello world"
//"""
//        config
//    |> should equal """let result = lazy (x + 10)"""
//
//[<Test>]
//let ``should detect members and format appropriately`` () =
//    formatSelectionOnly
//        false
//        (makeRange 4 0 5 32)
//        """
//type T () =
//  let items = []
//  override x.Reorder () =
//        items |> List.iter ignore"""
//        { config with MaxFunctionBindingWidth = 120 }
//    |> should equal """  override x.Reorder() = items |> List.iter ignore"""
//
//[<Test>]
//let ``should format the and branch of recursive functions`` () =
//    formatSelectionOnly
//        false
//        (makeRange 3 0 4 34)
//        """
//let rec createJArray x = createJObject
//
//and createJObject y = createJArray
//"""
//        config
//    |> should
//        equal
//        """and createJObject y = createJArray
//"""
//
//[<Test>]
//let ``should format recursive types correctly`` () =
//    formatSelectionOnly
//        false
//        (makeRange 7 0 10 48)
//        """
//type Folder(pathIn : string) =
//    let path = pathIn
//    let filenameArray : string array = System.IO.Directory.GetFiles(path)
//    member this.FileArray =
//        Array.map (fun elem -> new File(elem, this)) filenameArray
//
//and File(filename: string, containingFolder: Folder) =
//   member __.Name = filename
//   member __.ContainingFolder = containingFolder"""
//        { config with MaxValueBindingWidth = 120 }
//    |> prepend newline
//    |> should
//        equal
//        """
//and File(filename: string, containingFolder: Folder) =
//    member __.Name = filename
//    member __.ContainingFolder = containingFolder"""
//
//[<Test>]
//let ``should not add trailing whitespaces and preserve indentation`` () =
//    formatSelectionOnly
//        false
//        (makeRange 4 0 7 15)
//        """
//module Enums =
//    // Declaration of an enumeration.
//    type Colour =
//      | Red = 0
//      | Green = 1
//      | Blue = 2
//"""
//        config
//    |> prepend newline
//    |> should
//        equal
//        """
//    type Colour =
//        | Red = 0
//        | Green = 1
//        | Blue = 2"""
