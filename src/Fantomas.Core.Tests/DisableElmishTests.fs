module Fantomas.Core.Tests.DisableElmishTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config = { config with DisableElmishSyntax = true }

[<Test>]
let ``function call with two list argument`` () =
    formatSourceString
        false
        """
let v =
    someFunctionHere [parameters] [yetMoreParameters]
    :?> _
"""
        config
    |> prepend newline
    |> should
        equal
        """
let v = someFunctionHere [ parameters ] [ yetMoreParameters ] :?> _
"""

[<Test>]
let ``function with short array argument`` () =
    formatSourceString
        false
        """
InstanceObject.make [| a; b; c |]
"""
        { config with MaxElmishWidth = 10 }
    |> prepend newline
    |> should
        equal
        """
InstanceObject.make [| a; b; c |]
"""

[<Test>]
let ``function call with single multiline list argument`` () =
    formatSourceString
        false
        """
let  f x =
    Case.fromCaseTimeline [ { mockClaim with x = 5 }
                              |> Event.claim "42"
                              |> Commit.caseCommit System.DateTime.Now ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f x =
    Case.fromCaseTimeline
        [ { mockClaim with x = 5 }
          |> Event.claim "42"
          |> Commit.caseCommit System.DateTime.Now ]
"""

[<Test>]
let ``json encoders`` () =
    formatSourceString
        false
        """
module FantomasTools.Client.ASTViewer.Encoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let encodeUrlModel code model: JsonValue =
    Encode.object [ "defines", Encode.string model.Defines
                    "isFsi", Encode.bool model.IsFsi
                    "code", Encode.string code ]

let encodeInput (input: Input) =
    Encode.object [ "sourceCode", Encode.string input.SourceCode
                    "defines",
                    (Array.map Encode.string input.Defines
                     |> Encode.array)
                    "isFsi", Encode.bool input.IsFsi ]
    |> Encode.toString 2
"""
        config
    |> prepend newline
    |> should
        equal
        """
module FantomasTools.Client.ASTViewer.Encoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let encodeUrlModel code model : JsonValue =
    Encode.object
        [ "defines", Encode.string model.Defines
          "isFsi", Encode.bool model.IsFsi
          "code", Encode.string code ]

let encodeInput (input: Input) =
    Encode.object
        [ "sourceCode", Encode.string input.SourceCode
          "defines",
          (Array.map Encode.string input.Defines
           |> Encode.array)
          "isFsi", Encode.bool input.IsFsi ]
    |> Encode.toString 2
"""

[<Test>]
let ``encode arrays`` () =
    formatSourceString
        false
        """
type Event =
    | LocationAdded of AddLocation
    | LocationCancelled of Identifier
    | LocationNoLongerSellsRonnies of Identifier

    static member Encoder : Encoder<Event> =
        fun event ->
            match event with
            | LocationAdded addLocation ->
                Encode.array [| Encode.string "locationAdded"
                                AddLocation.Encoder addLocation |]
            | LocationCancelled id ->
                Encode.array [| Encode.string "locationCancelled"
                                (Identifier.Read >> Encode.guid) id |]
            | LocationNoLongerSellsRonnies id ->
                Encode.array [| Encode.string "locationNoLongerSellsRonnies"
                                (Identifier.Read >> Encode.guid) id |]
"""
        { config with
            SpaceBeforeColon = true
            MultilineBlockBracketsOnSameColumn = true }
    |> prepend newline
    |> should
        equal
        """
type Event =
    | LocationAdded of AddLocation
    | LocationCancelled of Identifier
    | LocationNoLongerSellsRonnies of Identifier

    static member Encoder : Encoder<Event> =
        fun event ->
            match event with
            | LocationAdded addLocation ->
                Encode.array
                    [|
                        Encode.string "locationAdded"
                        AddLocation.Encoder addLocation
                    |]
            | LocationCancelled id ->
                Encode.array
                    [|
                        Encode.string "locationCancelled"
                        (Identifier.Read >> Encode.guid) id
                    |]
            | LocationNoLongerSellsRonnies id ->
                Encode.array
                    [|
                        Encode.string "locationNoLongerSellsRonnies"
                        (Identifier.Read >> Encode.guid) id
                    |]
"""

[<Test>]
let ``elmish settings should not have any effect`` () =
    formatSourceString
        false
        """
let counter = React.functionComponent(fun () ->
    let (count, setCount) = React.useState(0)
    Html.div [
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount(count + 1))
            prop.text "Increment"
        ]
        Html.button [
            prop.style [ style.marginLeft 5 ]
            prop.onClick (fun _ -> setCount(count - 1))
            prop.text "Decrement"
        ]
        Html.h1 count
    ])
"""
        { config with
            MaxElmishWidth = 30
            SingleArgumentWebMode = true }
    |> prepend newline
    |> should
        equal
        """
let counter =
    React.functionComponent (fun () ->
        let (count, setCount) = React.useState (0)

        Html.div
            [ Html.button
                  [ prop.style [ style.marginRight 5 ]
                    prop.onClick (fun _ -> setCount (count + 1))
                    prop.text "Increment" ]
              Html.button
                  [ prop.style [ style.marginLeft 5 ]
                    prop.onClick (fun _ -> setCount (count - 1))
                    prop.text "Decrement" ]
              Html.h1 count ])
"""
