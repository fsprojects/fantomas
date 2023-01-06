module Fantomas.Core.Tests.Stroustrup.SynTypeDefnSimpleReprRecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core.FormatConfig

let config =
    { config with
        MultilineBracketStyle = ExperimentalStroustrup }

[<Test>]
let ``record type definition`` () =
    formatSourceString
        false
        """
type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = {
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
}
"""

[<Test>]
[<Ignore("See https://github.com/fsprojects/fantomas/issues/2001, this will be easier to fix in future FCS version")>]
let ``record type definition with comment after equals`` () =
    formatSourceString
        false
        """
type V = // comment
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = // comment
    {
        X: SomeFieldType
        Y: OhSomethingElse
        Z: ALongTypeName
    }
"""

// TODO: I feel like stroustrup should not work when there are members involved
// Having members would require the `with` keyword which is not recommended by the style guide: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-record-declarations

[<Test>]
let ``record type definition with members`` () =
    formatSourceString
        false
        """
type V =
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
    member this.Coordinate = (this.X, this.Y, this.Z)
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type V =
    {
        X: SomeFieldType
        Y: OhSomethingElse
        Z: ALongTypeName
    }
    member this.Coordinate = (this.X, this.Y, this.Z)
"""

[<Test>]
let ``record definition with private accessibility modifier, 2481`` () =
    formatSourceString
        false
        """
type Person = private {
    FirstName: string
    LastName: string
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Person = private {
    FirstName: string
    LastName: string
}
"""

[<Test>]
let ``record definition with internal accessibility modifier, 2481`` () =
    formatSourceString
        false
        """
type Person = internal {
    FirstName: string
    LastName: string
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Person = internal {
    FirstName: string
    LastName: string
}
"""

[<Test>]
let ``record definition with accessibility modifier with incorrect format, 2481`` () =
    formatSourceString
        false
        """
type Person = 
    private {
        FirstName: string
        LastName: string
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Person = private {
    FirstName: string
    LastName: string
}
"""

[<Test>]
let ``record definition with accessibility modifier with incorrect format, 2511`` () =
    formatSourceString
        false
        """
type NonEmptyList<'T> =
    private
        { List: 'T list }

    member this.Head = this.List.Head
    member this.Tail = this.List.Tail
    member this.Length = this.List.Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
type NonEmptyList<'T> =
    private
        {
            List: 'T list
        }

    member this.Head = this.List.Head
    member this.Tail = this.List.Tail
    member this.Length = this.List.Length
"""

[<Test>]
let ``outdenting problem when specifying record with accessibility modifier, 2597`` () =
    formatSourceString
        false
        """
module OutdentingProblem =
    type Configuration = private { Setting1: int; Setting2: bool }
        
    let withSetting1 value configuration =
        { configuration with Setting1 = value }
        
    let withSetting2 value configuration =
        { configuration with Setting2 = value }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module OutdentingProblem =
    type Configuration = private {
        Setting1: int
        Setting2: bool
    }

    let withSetting1 value configuration = { configuration with Setting1 = value }

    let withSetting2 value configuration = { configuration with Setting2 = value }
"""

[<Test>]
let ``nested anonymous record in type definition, 2413`` () =
    formatSourceString
        false
        """
type MangaDexAtHomeResponse = {
    baseUrl: string
    chapter: {|
        hash: string
        data: string[]
        otherThing: int
    |}
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MangaDexAtHomeResponse = {
    baseUrl: string
    chapter: {|
        hash: string
        data: string[]
        otherThing: int
    |}
}
"""
