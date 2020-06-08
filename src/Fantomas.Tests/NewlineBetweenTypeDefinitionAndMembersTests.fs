module Fantomas.Tests.NewlineBetweenTypeDefinitionAndMembersTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = { config with NewlineBetweenTypeDefinitionAndMembers = true }

[<Test>]
let ``newline between record type and members`` () =
    formatSourceString false """type Range =
    { From : float
      To : float
      Name: string }
    member this.Length = this.To - this.From
"""  { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type Range =
    { From: float
      To: float
      Name: string }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``existing newline between record type and members should not be duplicate`` () =
    formatSourceString false """type Range =
    { From : float
      To : float
      Name: string }

    member this.Length = this.To - this.From
"""  { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type Range =
    { From: float
      To: float
      Name: string }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``no extra newline after record type with no members`` () =
    formatSourceString false """type Range =
    { From : float
      To : float
      Name: string }
"""  config
    |> prepend newline
    |> should equal """
type Range =
    { From: float
      To: float
      Name: string }
"""

[<Test>]
let ``union type with members`` () =
    formatSourceString false """type Color =
    | Red
    | Green
    | Blue
    member this.ToInt = ()
"""  config
    |> prepend newline
    |> should equal """
type Color =
    | Red
    | Green
    | Blue

    member this.ToInt = ()
"""

[<Test>]
let ``enum type with members`` () =
    formatSourceString false """type Color =
    | Red = 0
    | Green = 1
    | Blue = 2
    member this.ToInt = ()
"""  config
    |> prepend newline
    |> should equal """
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member this.ToInt = ()
"""

[<Test>]
let ``type abbreviation with members`` () =
    formatSourceString false """
type A = string
with
    member this.X = ()
"""  config
    |> prepend newline
    |> should equal """
type A = string
    with

        member this.X = ()
"""

[<Test>]
let ``type augmentation with members`` () =
    formatSourceString false """
type HttpContext with
    member this.QueryString () = "?"
"""  config
    |> prepend newline
    |> should equal """
type HttpContext with

    member this.QueryString() = "?"
"""

[<Test>]
let ``newline between record type signature and members`` () =
    formatSourceString true """namespace Signature

type Range =
    { From : float
      To : float
      Name: string }
    member Length : unit -> int
"""  config
    |> prepend newline
    |> should equal """
namespace Signature

type Range =
    { From: float
      To: float
      Name: string }

    member Length: unit -> int
"""

[<Test>]
let ``union signature type with members`` () =
    formatSourceString true """namespace Signature
type Color =
    | Red
    | Green
    | Blue
    member ToInt: unit -> int
"""  config
    |> prepend newline
    |> should equal """
namespace Signature

type Color =
    | Red
    | Green
    | Blue

    member ToInt: unit -> int
"""

[<Test>]
let ``enum signature type with members`` () =
    formatSourceString true """namespace Signature
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2
    member ToInt : unit -> int
"""  config
    |> prepend newline
    |> should equal """
namespace Signature

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member ToInt: unit -> int
"""

[<Test>]
let ``simple type in signature file with members`` () =
    formatSourceString true """namespace Signature
type HttpContext with
    member QueryString : unit -> string
"""  config
    |> prepend newline
    |> should equal """
namespace Signature

type HttpContext with

    member QueryString: unit -> string
"""
