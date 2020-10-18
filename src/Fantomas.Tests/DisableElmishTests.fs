module Fantomas.Tests.DisableElmishTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          DisableElmishSyntax = true }

[<Test>]
let ``function call with two list argument`` () =
    formatSourceString false """
let v =
    someFunctionHere [parameters] [yetMoreParameters]
    :?> _
"""  config
    |> prepend newline
    |> should equal """
let v =
    someFunctionHere [ parameters ] [ yetMoreParameters ] :?> _
"""

[<Test>]
let ``function with short array argument`` () =
    formatSourceString false """
InstanceObject.make [| a; b; c |]
"""  { config with MaxElmishWidth = 10 }
    |> prepend newline
    |> should equal """
InstanceObject.make [| a; b; c |]
"""

[<Test>]
let ``function call with single multiline list argument`` () =
    formatSourceString false """
let  f x =
    Case.fromCaseTimeline [ { mockClaim with x = 5 }
                              |> Event.claim "42"
                              |> Commit.caseCommit System.DateTime.Now ]
"""  config
    |> prepend newline
    |> should equal """
let f x =
    Case.fromCaseTimeline
        [ { mockClaim with x = 5 }
          |> Event.claim "42"
          |> Commit.caseCommit System.DateTime.Now ]
"""

[<Test>]
let ``json encoders`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
module FantomasTools.Client.ASTViewer.Encoders

open ASTViewer.Shared
open FantomasTools.Client.ASTViewer.Model
open Thoth.Json

let encodeUrlModel code model: JsonValue =
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
