module Fantomas.Tests.MultilineBlockBracketsOnSameColumnRecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config = ({ config with
                    MultilineBlockBracketsOnSameColumn = true
                    SpaceBeforeColon = true
                    SpaceBeforeSemicolon = true
                    NewlineBetweenTypeDefinitionAndMembers = true })

[<Test>]
let ``single member record stays on one line`` () =
    formatSourceString false """let a = { Foo = "bar" }
"""  config
    |> prepend newline
    |> should equal """
let a = { Foo = "bar" }
"""

[<Test>]
let ``record instance`` () =
    formatSourceString false """let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = "bar"
      Street = "Bakerstreet"
      Number = 42 }
"""  config
    |> prepend newline
    |> should equal """
let myRecord =
    {
        Level = 1
        Progress = "foo"
        Bar = "bar"
        Street = "Bakerstreet"
        Number = 42
    }
"""

[<Test>]
let ``nested record`` () =
    formatSourceString false """let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = { Zeta = "bar" }
      Address =
          { Street = "Bakerstreet"
            ZipCode = "9000" }
      Number = 42 }
"""  config
    |> prepend newline
    |> should equal """
let myRecord =
    {
        Level = 1
        Progress = "foo"
        Bar = { Zeta = "bar" }
        Address =
            {
                Street = "Bakerstreet"
                ZipCode = "9000"
            }
        Number = 42
    }
"""

[<Test>]
let ``update record`` () =
    formatSourceString false """let myRecord =
    { myOldRecord
        with Level = 2
             Bar = "barry"
             Progress = "fooey" }
"""  config
    |> prepend newline
    |> should equal """
let myRecord =
    { myOldRecord with
        Level = 2
        Bar = "barry"
        Progress = "fooey"
    }
"""

[<Test>]
let ``update record with single field`` () =
    formatSourceString false """let myRecord =
    { myOldRecord
        with Level = 2 }
"""  config
    |> prepend newline
    |> should equal """
let myRecord = { myOldRecord with Level = 2 }
"""

[<Test>]
let ``record instance with inherit keyword`` () =
    formatSourceString false """let a =
        { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
          buildSettings = FSharpBuildSettings()
          targetPlatformData = targetPlatformData }
"""  config
    |> prepend newline
    |> should equal """
let a =
    {
        inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
        buildSettings = FSharpBuildSettings()
        targetPlatformData = targetPlatformData
    }
"""

[<Test>]
let ``record instance with inherit keyword and no fields`` () =
    formatSourceString false """let a =
        { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK) }
"""  config
    |> prepend newline
    |> should equal """
let a =
    { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK) }
"""

[<Test>]
let ``type with record instance with inherit keyword`` () =
    formatSourceString false """type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        { inherit CommunicationUnsuccessfulException(message) }"""  config
    |> prepend newline
    |> should equal """
type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) = { inherit CommunicationUnsuccessfulException(message) }
"""

[<Test>]
let ``anonymous record`` () =
    formatSourceString false """let meh =
    {| Level = 1
       Progress = "foo"
       Bar = "bar"
       Street = "Bakerstreet"
       Number = 42 |}
"""  config
    |> prepend newline
    |> should equal """
let meh =
    {|
        Level = 1
        Progress = "foo"
        Bar = "bar"
        Street = "Bakerstreet"
        Number = 42
    |}
"""

[<Test>]
let ``anonymous record with single field update`` () =
    formatSourceString false """let a = {| foo with Level = 7 |}
"""  config
    |> prepend newline
    |> should equal """
let a = {| foo with Level = 7 |}
"""

[<Test>]
let ``anonymous record with multiple field update`` () =
    formatSourceString false """let a = {| foo with Level = 7; Square = 9 |}
"""  ({ config with MaxRecordWidth = 35 })
    |> prepend newline
    |> should equal """
let a =
    {| foo with
        Level = 7
        Square = 9
    |}
"""

[<Test>]
let ``anonymous type`` () =
    formatSourceString false """type a = {| foo : string; bar : string |}
"""  config
    |> prepend newline
    |> should equal """
type a = {| foo : string ; bar : string |}
"""

[<Test>]
let ``anonymous record with single field`` () =
    formatSourceString false """let a = {| A = "meh" |}
"""  config
    |> prepend newline
    |> should equal """
let a = {| A = "meh" |}
"""

[<Test>]
let ``anonymous record with child records`` () =
    formatSourceString false """
let anonRecord =
    {| A = {| A1 = "string";A2LongerIdentifier = "foo" |};
       B = {| B1 = 7 |}
       C= { C1 = "foo"; C2LongerIdentifier = "bar"}
       D = { D1 = "bar" } |}
"""  config
    |> prepend newline
    |> should equal """
let anonRecord =
    {|
        A =
            {|
                A1 = "string"
                A2LongerIdentifier = "foo"
            |}
        B = {| B1 = 7 |}
        C =
            {
                C1 = "foo"
                C2LongerIdentifier = "bar"
            }
        D = { D1 = "bar" }
    |}
"""

[<Test>]
let ``record as parameter to function`` () =
    formatSourceString false """let configurations =
    buildConfiguration { XXXXXXXXXXXX = "XXXXXXXXXXXXX"; YYYYYYYYYYYY = "YYYYYYYYYYYYYYY" }
"""  config
    |> prepend newline
    |> should equal """
let configurations =
    buildConfiguration
        {
            XXXXXXXXXXXX = "XXXXXXXXXXXXX"
            YYYYYYYYYYYY = "YYYYYYYYYYYYYYY"
        }
"""

[<Test>]
let ``records in list`` () =
    formatSourceString false """let configurations =
    [
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
        { Build = true; Configuration = "UNKNOWN"; Defines = [] }
    ]
"""  config
    |> prepend newline
    |> should equal """
let configurations =
    [
        {
            Build = true
            Configuration = "RELEASE"
            Defines = [ "FOO" ]
        }
        {
            Build = true
            Configuration = "DEBUG"
            Defines = [ "FOO" ; "BAR" ]
        }
        {
            Build = true
            Configuration = "UNKNOWN"
            Defines = []
        }
    ]
"""

[<Test>]
let ``anonymous records in list`` () =
    formatSourceString false """let configurations =
    [
        {| Build = true; Configuration = "RELEASE"; Defines = ["FOO"] |}
        {| Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] |}
    ]
"""  config
    |> prepend newline
    |> should equal """
let configurations =
    [
        {|
            Build = true
            Configuration = "RELEASE"
            Defines = [ "FOO" ]
        |}
        {|
            Build = true
            Configuration = "DEBUG"
            Defines = [ "FOO" ; "BAR" ]
        |}
    ]
"""

[<Test>]
let ``records in array`` () =
    formatSourceString false """let configurations =
    [|
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
    |]
"""  config
    |> prepend newline
    |> should equal """
let configurations =
    [|
        {
            Build = true
            Configuration = "RELEASE"
            Defines = [ "FOO" ]
        }
        {
            Build = true
            Configuration = "DEBUG"
            Defines = [ "FOO" ; "BAR" ]
        }
    |]
"""

[<Test>]
let ``object expression`` () =
    formatSourceString false """
let obj1 = { new System.Object() with member x.ToString() = "F#" }
"""  config
    |> prepend newline
    |> should equal """
let obj1 =
    { new System.Object() with
        member x.ToString() = "F#"
    }
"""

[<Test>]
let ``object expressions in list`` () =
    formatSourceString false """
let a =
    [
        { new System.Object() with member x.ToString() = "F#" }
        { new System.Object() with member x.ToString() = "C#" }
    ]
"""  config
    |> prepend newline
    |> should equal """
let a =
    [
        { new System.Object() with
            member x.ToString() = "F#"
        }
        { new System.Object() with
            member x.ToString() = "C#"
        }
    ]
"""

[<Test>]
let ``record type signature with bracketOnSeparateLine`` () =
    formatSourceString true """
module RecordSignature
/// Represents simple XML elements.
type Element =
    {
      /// The attribute collection.
      Attributes: IDictionary<Name, string>;

      /// The children collection.
      Children: seq<INode>;

      /// The qualified name.
      Name: Name }
"""  config
    |> prepend newline
    |> should equal """
module RecordSignature
/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes : IDictionary<Name, string>

        /// The children collection.
        Children : seq<INode>

        /// The qualified name.
        Name : Name
    }
"""

[<Test>]
let ``record type with member definitions should align with bracket`` () =
    formatSourceString false """
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
"""  { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type Range =
    {
        From : float
        To : float
    }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``record type with interface`` () =
    formatSourceString false """
type MyRecord =
    { SomeField : int
    }
    interface IMyInterface
"""  config
    |> prepend newline
    |> should equal """
type MyRecord =
    {
        SomeField : int
    }

    interface IMyInterface
"""

[<Test>]
let ``SynPat.Record in pattern match with bracketOnSeparateLine`` () =
    formatSourceString false """match foo with
| { Bar = bar; Level = 12; Vibes = plenty; Lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. " } -> "7"
| _ -> "8"
"""  config
    |> prepend newline
    |> should equal """
match foo with
| { Bar = bar ; Level = 12 ; Vibes = plenty ;
    Lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. " } ->
    "7"
| _ -> "8"
"""

[<Test>]
let ``record declaration`` () =
    formatSourceString false """type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
"""  config
    |> prepend newline
    |> should equal """
type MyRecord =
    {
        Level : int
        Progress : string
        Bar : string
        Street : string
        Number : int
    }
"""

[<Test>]
let ``record declaration in signature file`` () =
    formatSourceString true """namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
"""  config
    |> prepend newline
    |> should equal """
namespace X

type MyRecord =
    {
        Level : int
        Progress : string
        Bar : string
        Street : string
        Number : int
    }
"""

[<Test>]
let ``record declaration with members in signature file`` () =
    formatSourceString true """namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
    member Score : unit -> int
"""  config
    |> prepend newline
    |> should equal """
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
"""


[<Test>]
let ``no newline before first multiline member`` () =
    formatSourceString false """
type ShortExpressionInfo =
    { MaxWidth: int
      StartColumn: int
      ConfirmedMultiline: bool }
    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width
    member x.Foo() = ()
"""  ({ config with NewlineBetweenTypeDefinitionAndMembers = false })
    |> prepend newline
    |> should equal """
type ShortExpressionInfo =
    {
        MaxWidth : int
        StartColumn : int
        ConfirmedMultiline : bool
    }
    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn
        - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width

    member x.Foo() = ()
"""

[<Test>]
let ``internal keyword before multiline record type`` () =
    formatSourceString false """
    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }""" config
    |> prepend newline
    |> should equal """
type A =
    internal
        {
            ALongIdentifier : string
            YetAnotherLongIdentifier : bool
        }
"""

[<Test>]
let ``internal keyword before multiline record type in signature file`` () =
    formatSourceString true """namespace Bar

    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }""" config
    |> prepend newline
    |> should equal """
namespace Bar

type A =
    internal
        {
            ALongIdentifier : string
            YetAnotherLongIdentifier : bool
        }
"""

[<Test>]
let ``indent update record fields far enough, 817`` () =
    formatSourceString false "let expected = { ThisIsAThing.Empty with TheNewValue = 1 }" ({ config with IndentSize = 2 })
    |> prepend newline
    |> should equal """
let expected =
  { ThisIsAThing.Empty with
      TheNewValue = 1
  }
"""

[<Test>]
let ``indent update anonymous record fields far enough`` () =
    formatSourceString false "let expected = {| ThisIsAThing.Empty with TheNewValue = 1 |}" ({ config with IndentSize = 2 })
    |> prepend newline
    |> should equal """
let expected =
  {| ThisIsAThing.Empty with
      TheNewValue = 1
  |}
"""

[<Test>]
let ``update record with standard indent`` () =
    formatSourceString false "let expected = { ThisIsAThing.Empty with TheNewValue = 1 }" config
    |> prepend newline
    |> should equal """
let expected =
    { ThisIsAThing.Empty with
        TheNewValue = 1
    }
"""

[<Test>]
let ``record type with attributes`` () =
    formatSourceString false """
[<Foo>]
type Args =
    { [<Foo "">]
      [<Bar>]
      [<Baz 1>]
      Hi: int list }

module Foo =

    let r = 3
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]
type Args =
    {
        [<Foo "">]
        [<Bar>]
        [<Baz 1>]
        Hi : int list
    }

module Foo =

    let r = 3
"""

[<Test>]
let ``comment before access modifier of record type declaration`` () =
    formatSourceString false """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
        }
"""  config
    |> prepend newline
    |> should equal """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
        }
"""
