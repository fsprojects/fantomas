module Fantomas.Tests.LambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``keep comment after arrow`` () =
    formatSourceString false """_Target "FSharpTypesDotNet" (fun _ -> // obsolete
 ())
"""  ({ config with IndentSpaceNum = 2; PageWidth = 90 })
    |> prepend newline
    |> should equal """
_Target "FSharpTypesDotNet" (fun _ -> // obsolete
  ())
"""

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

[<Test>]
let ``FAKE target`` () =
    formatSourceString false """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir
)
"""  config
    |> prepend newline
    |> should equal """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir)
"""

[<Test>]
let ``destructed argument lamba`` () =
    formatSourceString false """
List.filter (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""  config
    |> prepend newline
    |> should equal """
List.filter (fun { ContentBefore = contentBefore } ->
    // some comment
    let a = 8
    let b = List.length contentBefore
    a + b)
"""

[<Test>]
let ``destructed argument lamba in let binding`` () =
    formatSourceString false """
let a =
    (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""  config
    |> prepend newline
    |> should equal """
let a =
    (fun { ContentBefore = contentBefore } ->
        // some comment
        let a = 8
        let b = List.length contentBefore
        a + b)
"""


[<Test>]
let ``indent when identifier is smaller than ident size`` () =
    formatSourceString false """
foo (fun a ->
                let b = 8
                b)
"""  config
    |> prepend newline
    |> should equal """
foo (fun a ->
        let b = 8
        b)
"""

[<Test>]
let ``short ident in nested let binding`` () =
    formatSourceString false """let a =
    foo (fun a ->
                let b = 8
                b)
"""  ({ config with IndentSpaceNum = 2})
    |> prepend newline
    |> should equal """
let a =
  foo (fun a ->
    let b = 8
    b)
"""

[<Test>]
let ``longer ident in nested let binding`` () =
    formatSourceString false """let a =
    foobar (fun a ->
                let b = 8
                b)
"""  config
    |> prepend newline
    |> should equal """
let a =
    foobar (fun a ->
        let b = 8
        b)
"""

[<Test>]
let ``multiple braces should add indent`` () =
    formatSourceString false """((((fun () ->
    printfn "meh"
    ()))))
"""  config
    |> prepend newline
    |> should equal """
((((fun () ->
    printfn "meh"
    ()))))
"""

[<Test>]
let ``add space after chained ident, 676`` () =
    formatSourceString false """let foo = Foo(fun () -> Foo.Create x).Value"""  config
    |> prepend newline
    |> should equal """
let foo = Foo(fun () -> Foo.Create x).Value
"""