module Fantomas.Tests.LetBindingTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``let in should be preserved``() =
    formatSourceString false "let x = 1 in ()" config
    |> should equal """let x = 1 in ()
"""

[<Test>]
let ``multiple let in lines, should remove in`` () =
    let codeSnippet = """
let f () = 
  let x = 1 in   // the "in" keyword is available in F#
    let y = 2 in 
      x + y
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1 // the "in" keyword is available in F#
    let y = 2
    x + y
"""

[<Test>]
let ``multiple let in lines, should remove in, block comment`` () =
    let codeSnippet = """
let f () = 
  let x = 1 in   (* the "in" keyword is available in F# *)
    let y = 2 in 
      x + y
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1 (* the "in" keyword is available in F# *)
    let y = 2
    x + y
"""

[<Test>]
let ``multiline let in, should remove in`` () =
    let codeSnippet = """
let f () =
  let x = 1 in if true 
               then x
               else x
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1
    if true then x
    else x
"""

[<Test>]
let ``multiline let in, should remove in 2`` () =
    let codeSnippet = """
let f () =
  let x = 1 in (while true do ()
                x)
"""

    formatSourceString false codeSnippet config
    |> should equal """let f() =
    let x = 1
    (while true do
        ()
     x)
"""

[<Test>]
let ``DotGet on newline should be indented far enough`` () =
    formatSourceString false """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""  config
    |> prepend newline
    |> should equal """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""

[<Test>]
let ``DotGet on newline after empty string should be indented far enough`` () =
    formatSourceString false """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""  config
    |> prepend newline
    |> should equal """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
            let num =
                ""
                    .PadLeft(9)
            num)
"""

[<Test>]
let ``newlines between let bindings should preserved`` () =
    formatSourceString false """
let a = 42



let b = "meh"
"""  config
    |> should equal """let a = 42



let b = "meh"
"""

[<Test>]
let ``Raw method names with `/` `` () =
    formatSourceString false "let ``/ operator combines paths`` = x" config
    |> should equal """let ``/ operator combines paths`` = x
"""

[<Test>]
let ``newline before let inside let should not be duplicated`` () =
    formatSourceString false """namespace ReactStrap

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

[<RequireQualifiedAccess>]
module Card =
    type CardProps =
        | Tag of U2<string, obj>
        | Inverse of bool
        | Outline of bool
        | Color of Common.Color
        | Body of bool
        | Custom of IHTMLProp list

    let card (props: CardProps seq) (elems: ReactElement seq): ReactElement =
        let customProps =
            props
            |> Seq.collect (function
                | Custom props -> props
                | _ -> List.empty)
            |> keyValueList CaseRules.LowerFirst

        let typeProps =
            props
            |> Seq.choose (function
                | Custom _ -> None
                | prop -> Some prop)
            |> keyValueList CaseRules.LowerFirst

        let props = JS.Object.assign (createEmpty, customProps, typeProps)
        ofImport "Card" "reactstrap" props elems"""  config
        |> should equal """namespace ReactStrap

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

[<RequireQualifiedAccess>]
module Card =
    type CardProps =
        | Tag of U2<string, obj>
        | Inverse of bool
        | Outline of bool
        | Color of Common.Color
        | Body of bool
        | Custom of IHTMLProp list

    let card (props: CardProps seq) (elems: ReactElement seq): ReactElement =
        let customProps =
            props
            |> Seq.collect (function
                | Custom props -> props
                | _ -> List.empty)
            |> keyValueList CaseRules.LowerFirst

        let typeProps =
            props
            |> Seq.choose (function
                | Custom _ -> None
                | prop -> Some prop)
            |> keyValueList CaseRules.LowerFirst

        let props = JS.Object.assign (createEmpty, customProps, typeProps)
        ofImport "Card" "reactstrap" props elems
"""

[<Test>]
let ``newlines inside let binding should be not duplicated`` () =
    formatSourceString false """let foo =
    let next _ =
        if not animating then activeIndex.update ((activeIndex.current + 1) % itemLength)

    let prev _ =
        if not animating then activeIndex.update ((activeIndex.current + itemLength - 1) % itemLength)

    ()
"""  config
    |> should equal """let foo =
    let next _ =
        if not animating then activeIndex.update ((activeIndex.current + 1) % itemLength)

    let prev _ =
        if not animating then activeIndex.update ((activeIndex.current + itemLength - 1) % itemLength)

    ()
"""