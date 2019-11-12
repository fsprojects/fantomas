module Fantomas.Tests.LambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``indent multiline lambda in parenthesis, 523`` () =
    formatSourceString false """let square = (fun b ->
    b*b
    prinftn "%i" b*b
)
"""  config
    |> prepend newline
    |> should equal """
let square =
    (fun b ->
        b * b
        prinftn "%i" b * b)
"""

[<Test>]
let ``lambda inside tupled argument`` () =
    formatSourceString false """#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment []
                [ h3 []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ]]), "BadgeSample")

exportDefault badgeSample
"""  config
    |> prepend newline
    |> should equal """
#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment []
                [ h3 []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ] ]), "BadgeSample")

exportDefault badgeSample
"""

[<Test>]
let ``long identifier inside lambda`` () =
    formatSourceString false """
let a =
    b
    |> List.exists (fun p ->
        x && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""  ({ config with PageWidth = 80})
    |> prepend newline
    |> should equal """
let a =
    b
    |> List.exists (fun p ->
        x
        && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""