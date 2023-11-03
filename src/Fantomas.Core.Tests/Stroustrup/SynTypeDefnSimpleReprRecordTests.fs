module Fantomas.Core.Tests.Stroustrup.SynTypeDefnSimpleReprRecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup }

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
let ``anonymous record type alias`` () =
    formatSourceString
        false
        """
type V =
    {| X: SomeFieldType
       Y: OhSomethingElse
       Z: ALongTypeName |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = {|
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
|}
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
type V = {
    X: SomeFieldType
    Y: OhSomethingElse
    Z: ALongTypeName
} with
    member this.Coordinate = (this.X, this.Y, this.Z)
"""

[<Test>]
let ``record type definition with members and trivia`` () =
    formatSourceString
        false
        """
type X = {
    Y: int
} with // foo
    member x.Z = ()
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type X = {
    Y: int
} with // foo
    member x.Z = ()
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
type NonEmptyList<'T> = private {
    List: 'T list
} with

    member this.Head = this.List.Head
    member this.Tail = this.List.Tail
    member this.Length = this.List.Length
"""

[<Test>]
let ``record definition with accessibility modifier without members`` () =
    formatSourceString
        false
        """
type NonEmptyList<'T> =
    private
        { List: 'T list; Value: 'T; Third: string}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type NonEmptyList<'T> = private {
    List: 'T list
    Value: 'T
    Third: string
}
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

[<Test>]
let ``record interface declarations can break with Stroustrup enabled, 2787`` () =
    formatSourceString
        false
        """
type IEvent = interface end

type SomeEvent =
    { Id: string
      Name: string }
    interface IEvent

type UpdatedName = { PreviousName: string }
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type IEvent = interface end

type SomeEvent = {
    Id: string
    Name: string
} with
    interface IEvent

type UpdatedName = { PreviousName: string }
"""

[<Test>]
let ``record member declarations can break with Stroustrup enabled, 2787`` () =
    formatSourceString
        false
        """
type SomeEvent =
    { Id: string
      Name: string }
    member x.BreakWithOtherStuffAs well = ()

type UpdatedName = { PreviousName: string }
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type SomeEvent = {
    Id: string
    Name: string
} with
    member x.BreakWithOtherStuffAs well = ()

type UpdatedName = { PreviousName: string }
"""

[<Test>]
let ``comment above record bracket breaks formatting when Stroustrup enabled, 2871`` () =
    formatSourceString
        false
        """
type Event =
    // TODO: Add LulaSafe conclusion and scores per assessment
    {
        Metadata: AssessmentMetadata
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Event =
    // TODO: Add LulaSafe conclusion and scores per assessment
    {
        Metadata: AssessmentMetadata
    }
"""
