module Fantomas.Tests.GResearchRecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let configForGResearch = ({ config with GResearch = true })

[<Test>]
let ``single member record stays on oneline`` () =
    formatSourceString false """let a = { Foo = "bar" }
"""  configForGResearch
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
"""  configForGResearch
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
"""  configForGResearch
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
let myRecord =
    {
        myOldRecord with
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
let myRecord = { myOldRecord with Level = 2 }
"""

[<Test>]
let ``anonymous record`` () =
    formatSourceString false """let meh =
    {| Level = 1
       Progress = "foo"
       Bar = "bar"
       Street = "Bakerstreet"
       Number = 42 |}
"""  configForGResearch
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

// This is meant to be a short type alias, we format this always as one-liner.
// TDB with G-Research
[<Test>]
let ``anonymous type`` () =
    formatSourceString false """type a = {| foo : string; bar : string |}
"""  configForGResearch
    |> prepend newline
    |> should equal """
type a = {| foo: string; bar: string |}
"""

[<Test>]
let ``anonymous record with single field`` () =
    formatSourceString false """let a = {| A = "meh" |}
"""  configForGResearch
    |> prepend newline
    |> should equal """
let a = {| A = "meh" |}
"""

[<Test>]
let ``anonymous record with child records`` () =
    formatSourceString false """
let anonRecord =
    {| A = {| A1 = "string";A2 = "foo" |};
       B = {| B1 = 7 |}
       C= { C1 = "foo"; C2 = "bar"}
       D = { D1 = "bar" } |}
"""  configForGResearch
    |> prepend newline
    |> should equal """
let anonRecord =
    {|
        A =
            {|
                A1 = "string"
                A2 = "foo"
            |}
        B = {| B1 = 7 |}
        C =
            {
                C1 = "foo"
                C2 = "bar"
            }
        D = { D1 = "bar" }
    |}
"""

[<Test>]
let ``record as parameter to function`` () =
    formatSourceString false """let configurations =
    buildConfiguration { XXXXXXXXXXXX = "XXXXXXXXXXXXX"; YYYYYYYYYYYY = "YYYYYYYYYYYYYYY" }
"""  configForGResearch
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
let configurations =
    [ {
          Build = true
          Configuration = "RELEASE"
          Defines = [ "FOO" ]
      }
      {
          Build = true
          Configuration = "DEBUG"
          Defines = [ "FOO"; "BAR" ]
      }
      {
          Build = true
          Configuration = "UNKNOWN"
          Defines = []
      } ]
"""

[<Test>]
let ``anonymous records in list`` () =
    formatSourceString false """let configurations =
    [
        {| Build = true; Configuration = "RELEASE"; Defines = ["FOO"] |}
        {| Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] |}
    ]
"""  configForGResearch
    |> prepend newline
    |> should equal """
let configurations =
    [ {|
          Build = true
          Configuration = "RELEASE"
          Defines = [ "FOO" ]
      |}
      {|
          Build = true
          Configuration = "DEBUG"
          Defines = [ "FOO"; "BAR" ]
      |} ]
"""

[<Test>]
let ``records in array`` () =
    formatSourceString false """let configurations =
    [|
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
    |]
"""  configForGResearch
    |> prepend newline
    |> should equal """
let configurations =
    [| {
           Build = true
           Configuration = "RELEASE"
           Defines = [ "FOO" ]
       }
       {
           Build = true
           Configuration = "DEBUG"
           Defines = [ "FOO"; "BAR" ]
       } |]
"""

[<Test>]
let ``object expression`` () =
    formatSourceString false """
let obj1 = { new System.Object() with member x.ToString() = "F#" }
"""  configForGResearch
    |> prepend newline
    |> should equal """
let obj1 =
    {
        new System.Object() with
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
let a =
    [ {
          new System.Object() with
              member x.ToString() = "F#"
      }
      {
          new System.Object() with
              member x.ToString() = "C#"
      } ]
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
module RecordSignature
/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes: IDictionary<Name, string>

        /// The children collection.
        Children: seq<INode>

        /// The qualified name.
        Name: Name
    }
"""

// We don't add any newlines when the record is used in a pattern match
// with G-Research
[<Test>]
let ``SynPat.Record in pattern match with bracketOnSeparateLine`` () =
    formatSourceString false """match foo with
| { Bar = bar; Level = 12; Vibes = plenty; Lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. " } -> "7"
| _ -> "8"
"""  configForGResearch
    |> prepend newline
    |> should equal """
match foo with
| { Bar = bar; Level = 12; Vibes = plenty;
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
type MyRecord =
    {
        Level: int
        Progress: string
        Bar: string
        Street: string
        Number: int
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
"""  configForGResearch
    |> prepend newline
    |> should equal """
namespace X

type MyRecord =
    {
        Level: int
        Progress: string
        Bar: string
        Street: string
        Number: int
    }
"""