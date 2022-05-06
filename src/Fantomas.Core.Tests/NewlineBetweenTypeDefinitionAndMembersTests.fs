module Fantomas.Core.Tests.NewlineBetweenTypeDefinitionAndMembersTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let config = { config with NewlineBetweenTypeDefinitionAndMembers = true }

[<Test>]
let ``newline between record type and members`` () =
    formatSourceString
        false
        """type Range =
    { From : float
      To : float
      Name: string }
    member this.Length = this.To - this.From
"""
        { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Range =
    { From: float
      To: float
      Name: string }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``existing newline between record type and members should not be duplicate`` () =
    formatSourceString
        false
        """type Range =
    { From : float
      To : float
      Name: string }

    member this.Length = this.To - this.From
"""
        { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Range =
    { From: float
      To: float
      Name: string }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``no extra newline after record type with no members`` () =
    formatSourceString
        false
        """type Range =
    { From : float
      To : float
      Name: string }
"""
        { config with MaxRecordWidth = 39 }
    |> prepend newline
    |> should
        equal
        """
type Range =
    { From: float
      To: float
      Name: string }
"""

[<Test>]
let ``union type with members`` () =
    formatSourceString
        false
        """type Color =
    | Red
    | Green
    | Blue
    member this.ToInt = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Color =
    | Red
    | Green
    | Blue

    member this.ToInt = ()
"""

[<Test>]
let ``enum type with members`` () =
    formatSourceString
        false
        """type Color =
    | Red = 0
    | Green = 1
    | Blue = 2
    member this.ToInt = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member this.ToInt = ()
"""

[<Test>]
let ``type abbreviation with members`` () =
    formatSourceString
        false
        """
type A = string
with
    member this.X = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = string
    with

        member this.X = ()
"""

[<Test>]
let ``type augmentation with members`` () =
    formatSourceString
        false
        """
type HttpContext with
    member this.QueryString () = "?"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type HttpContext with

    member this.QueryString() = "?"
"""

[<Test>]
let ``newline between record type signature and members`` () =
    formatSourceString
        true
        """namespace Signature

type Range =
    { From : float
      To : float
      Name: string }
    member Length : unit -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Signature

type Range =
    { From: float
      To: float
      Name: string }

    member Length: unit -> int
"""

[<Test>]
let ``union signature type with members`` () =
    formatSourceString
        true
        """namespace Signature
type Color =
    | Red
    | Green
    | Blue
    member ToInt: unit -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Signature

type Color =
    | Red
    | Green
    | Blue

    member ToInt: unit -> int
"""

[<Test>]
let ``enum signature type with members`` () =
    formatSourceString
        true
        """namespace Signature
type Color =
    | Red = 0
    | Green = 1
    | Blue = 2
    member ToInt : unit -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Signature

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member ToInt: unit -> int
"""

[<Test>]
let ``simple type in signature file with members`` () =
    formatSourceString
        true
        """namespace Signature
type HttpContext with
    member QueryString : unit -> string
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Signature

type HttpContext with

    member QueryString: unit -> string
"""

[<Test>]
let ``existing new line between type and members in signature file, 1094`` () =
    formatSourceString
        true
        """
namespace X

type MyRecord =
    {
        Level : int
        Progress : string
        Bar : string
        Street : string
        Number : int
    }

    member Score : unit -> int

type MyRecord =
    {
        SomeField : int
    }

    interface IMyInterface

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member ToInt: unit -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace X

type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }

    member Score: unit -> int

type MyRecord =
    { SomeField: int }

    interface IMyInterface

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member ToInt: unit -> int
"""

[<Test>]
let ``newline before interface, 1346`` () =
    formatSourceString
        false
        """
type andSeq<'t> =
    | AndSeq of 't seq

    interface IEnumerable<'t> with
        member this.GetEnumerator(): Collections.IEnumerator =
            match this with
            | AndSeq xs -> xs.GetEnumerator() :> _
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = true }
    |> prepend newline
    |> should
        equal
        """
type andSeq<'t> =
    | AndSeq of 't seq

    interface IEnumerable<'t> with
        member this.GetEnumerator() : Collections.IEnumerator =
            match this with
            | AndSeq xs -> xs.GetEnumerator() :> _
"""

[<Test>]
let ``blank line before with keyword should be preserved`` () =
    formatSourceString
        false
        """
type A =
  | B of int
  | C

  with
    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    | B of int
    | C

    member this.GetB =
        match this with
        | B x -> x
        | _ -> failwith "shouldn't happen"
"""

[<Test>]
let ``blank line before and after with keyword should be preserved`` () =
    formatSourceString
        false
        """
type A =
  | B of int
  | C

  with

    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    | B of int
    | C


    member this.GetB =
        match this with
        | B x -> x
        | _ -> failwith "shouldn't happen"
"""

[<Test>]
let ``multiline abstract member without constraints, 2175`` () =
    formatSourceString
        false
        """
    type FuseSortFunctionItem =
        abstract Item: key: string -> U2<{| ``$``: string |}, ResizeArray<{| ``$``: string; idx: float |}>> with get, set
        abstract X : int
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
type FuseSortFunctionItem =
    abstract Item:
        key: string ->
            U2<{| ``$``: string |}, ResizeArray<{| ``$``: string
                                                   idx: float |}>> with get, set

    abstract X: int
"""
