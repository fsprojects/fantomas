module Fantomas.Core.Tests.AlignedMultilineBracketStyleTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Aligned
        SpaceBeforeColon = true
        SpaceBeforeSemicolon = true }

[<Test>]
let ``single member record stays on one line`` () =
    formatSourceString
        false
        """let a = { Foo = "bar" }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = { Foo = "bar" }
"""

[<Test>]
let ``record instance`` () =
    formatSourceString
        false
        """let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = "bar"
      Street = "Bakerstreet"
      Number = 42 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let myRecord =
    { Level = 1
      Progress = "foo"
      Bar = { Zeta = "bar" }
      Address =
          { Street = "Bakerstreet"
            ZipCode = "9000" }
      Number = 42 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let myRecord =
    { myOldRecord
        with Level = 2
             Bar = "barry"
             Progress = "fooey" }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myRecord =
    { myOldRecord with
        Level = 2
        Bar = "barry"
        Progress = "fooey"
    }
"""

[<Test>]
let ``update record with single field`` () =
    formatSourceString
        false
        """let myRecord =
    { myOldRecord
        with Level = 2 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myRecord = { myOldRecord with Level = 2 }
"""

[<Test>]
let ``record instance with inherit keyword`` () =
    formatSourceString
        false
        """let a =
        { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
          buildSettings = FSharpBuildSettings()
          targetPlatformData = targetPlatformData }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    {
        inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK)
        buildSettings = FSharpBuildSettings()
        targetPlatformData = targetPlatformData
    }
"""

[<Test>]
let ``record instance with inherit keyword and no fields`` () =
    formatSourceString
        false
        """let a =
        { inherit ProjectPropertiesBase<_>(projectTypeGuids, factoryGuid, targetFrameworkIds, dotNetCoreSDK) }
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let a =
    {
        inherit
            ProjectPropertiesBase<_>(
                projectTypeGuids,
                factoryGuid,
                targetFrameworkIds,
                dotNetCoreSDK
            )
    }
"""

[<Test>]
let ``type with record instance with inherit keyword`` () =
    formatSourceString
        false
        """type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        { inherit CommunicationUnsuccessfulException(message) }"""
        config
    |> prepend newline
    |> should
        equal
        """
type ServerCannotBeResolvedException =
    inherit CommunicationUnsuccessfulException

    new(message) =
        {
            inherit CommunicationUnsuccessfulException(message)
        }
"""

[<Test>]
let ``anonymous record`` () =
    formatSourceString
        false
        """let meh =
    {| Level = 1
       Progress = "foo"
       Bar = "bar"
       Street = "Bakerstreet"
       Number = 42 |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let a = {| foo with Level = 7 |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = {| foo with Level = 7 |}
"""

[<Test>]
let ``anonymous record with multiple field update`` () =
    formatSourceString
        false
        """let a = {| foo with Level = 7; Square = 9 |}
"""
        { config with MaxRecordWidth = 35 }
    |> prepend newline
    |> should
        equal
        """
let a =
    {| foo with
        Level = 7
        Square = 9
    |}
"""

[<Test>]
let ``anonymous type`` () =
    formatSourceString
        false
        """type a = {| foo : string; bar : string |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type a = {| foo : string ; bar : string |}
"""

[<Test>]
let ``anonymous record with single field`` () =
    formatSourceString
        false
        """let a = {| A = "meh" |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = {| A = "meh" |}
"""

[<Test>]
let ``anonymous record with child records`` () =
    formatSourceString
        false
        """
let anonRecord =
    {| A = {| A1 = "string";A2LongerIdentifier = "foo" |};
       B = {| B1 = 7 |}
       C= { C1 = "foo"; C2LongerIdentifier = "bar"}
       D = { D1 = "bar" } |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let configurations =
    buildConfiguration { XXXXXXXXXXXX = "XXXXXXXXXXXXX"; YYYYYYYYYYYY = "YYYYYYYYYYYYYYY" }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let configurations =
    buildConfiguration
        {
            XXXXXXXXXXXX = "XXXXXXXXXXXXX"
            YYYYYYYYYYYY = "YYYYYYYYYYYYYYY"
        }
"""

[<Test>]
let ``records in list`` () =
    formatSourceString
        false
        """let configurations =
    [
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
        { Build = true; Configuration = "UNKNOWN"; Defines = [] }
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let configurations =
    [
        {| Build = true; Configuration = "RELEASE"; Defines = ["FOO"] |}
        {| Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] |}
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let configurations =
    [|
        { Build = true; Configuration = "RELEASE"; Defines = ["FOO"] }
        { Build = true; Configuration = "DEBUG"; Defines = ["FOO";"BAR"] }
    |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let obj1 = { new System.Object() with member x.ToString() = "F#" }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let obj1 =
    { new System.Object() with
        member x.ToString() = "F#"
    }
"""

[<Test>]
let ``object expressions in list`` () =
    formatSourceString
        false
        """
let a =
    [
        { new System.Object() with member x.ToString() = "F#" }
        { new System.Object() with member x.ToString() = "C#" }
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        true
        """
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
type Range =
    { From: float
      To: float }
    member this.Length = this.To - this.From
"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Range =
    {
        From : float
        To : float
    }

    member this.Length = this.To - this.From
"""

[<Test>]
let ``record type with interface`` () =
    formatSourceString
        false
        """
type MyRecord =
    { SomeField : int
    }
    interface IMyInterface
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyRecord =
    {
        SomeField : int
    }

    interface IMyInterface
"""

[<Test>]
let ``SynPat.Record in pattern match with bracketOnSeparateLine`` () =
    formatSourceString
        false
        """match foo with
| { Bar = bar; Level = 12; Vibes = plenty; Lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. " } -> "7"
| _ -> "8"
"""
        config
    |> prepend newline
    |> should
        equal
        """
match foo with
| {
      Bar = bar
      Level = 12
      Vibes = plenty
      Lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
  } -> "7"
| _ -> "8"
"""

[<Test>]
let ``record declaration`` () =
    formatSourceString
        false
        """type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        true
        """namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
"""
        config
    |> prepend newline
    |> should
        equal
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
"""

[<Test>]
let ``record declaration with members in signature file`` () =
    formatSourceString
        true
        """namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
    member Score : unit -> int
"""
        config
    |> prepend newline
    |> should
        equal
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
"""

[<Test>]
let ``no newline before first multiline member`` () =
    formatSourceString
        false
        """
type ShortExpressionInfo =
    { MaxWidth: int
      StartColumn: int
      ConfirmedMultiline: bool }
    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width
    member x.Foo() = ()
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type ShortExpressionInfo =
    {
        MaxWidth : int
        StartColumn : int
        ConfirmedMultiline : bool
    }
    member x.IsTooLong maxPageWidth currentColumn =
        currentColumn - x.StartColumn > x.MaxWidth // expression is not too long according to MaxWidth
        || (currentColumn > maxPageWidth) // expression at current position is not going over the page width

    member x.Foo() = ()
"""

[<Test>]
let ``internal keyword before multiline record type`` () =
    formatSourceString
        false
        """
    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    internal
        {
            ALongIdentifier : string
            YetAnotherLongIdentifier : bool
        }
"""

[<Test>]
let ``internal keyword before multiline record type in signature file`` () =
    formatSourceString
        true
        """namespace Bar

    type A = internal { ALongIdentifier: string; YetAnotherLongIdentifier: bool }"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString false "let expected = { ThisIsAThing.Empty with TheNewValue = 1 }" { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let expected =
  { ThisIsAThing.Empty with
      TheNewValue = 1
  }
"""

[<Test>]
let ``indent update anonymous record fields far enough`` () =
    formatSourceString
        false
        "let expected = {| ThisIsAThing.Empty with TheNewValue = 1 |}"
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let expected =
  {| ThisIsAThing.Empty with
      TheNewValue = 1
  |}
"""

[<Test>]
let ``update record with standard indent`` () =
    formatSourceString false "let expected = { ThisIsAThing.Empty with TheNewValue = 1 }" config
    |> prepend newline
    |> should
        equal
        """
let expected =
    { ThisIsAThing.Empty with
        TheNewValue = 1
    }
"""

[<Test>]
let ``record type with attributes`` () =
    formatSourceString
        false
        """
[<Foo>]
type Args =
    { [<Foo "">]
      [<Bar>]
      [<Baz 1>]
      Hi: int list }

module Foo =

    let r = 3
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
        }
"""
        { config with MaxRecordWidth = 10 }
    |> prepend newline
    |> should
        equal
        """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
        }
"""

[<Test>]
let ``defines in record assignment, 968`` () =
    formatSourceString
        false
        """
let config = {
    title = "Fantomas"
    description = "Fantomas is a code formatter for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsprojects.github.io/fantomas/"
      #endif
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let config =
    {
        title = "Fantomas"
        description = "Fantomas is a code formatter for F#"
        theme_variant = Some "red"
        root_url =
#if WATCH
            "http://localhost:8080/"
#else
            "https://fsprojects.github.io/fantomas/"
#endif
    }
"""

[<Test>]
let ``comment after closing brace in nested record`` () =
    formatSourceString
        false
        """
let person =
    { Name = "James"
      Address = { Street = "Bakerstreet"; Number = 42 }  // end address
    } // end person
"""
        config
    |> prepend newline
    |> should
        equal
        """
let person =
    {
        Name = "James"
        Address = { Street = "Bakerstreet" ; Number = 42 } // end address
    } // end person
"""

[<Test>]
let ``line comments before access modifier of multiline record type`` () =
    formatSourceString
        true
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
            Barry: string
        }
"""
        { config with MaxRecordWidth = 10 }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
            Barry : string
        }
"""

[<Test>]
let ``line comments before access modifier of single line record type`` () =
    formatSourceString
        true
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Meh : TimeSpan
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Meh : TimeSpan
        }
"""

[<Test>]
let ``record inside pattern match, 1238`` () =
    formatSourceString
        false
        """
      module Foo =
          let Bar () =
              if x then
                  match foo with
                  | { Bar = true
                      Baz = _ } -> failwith "xxx"
                  | _ -> None
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let Bar () =
        if x then
            match foo with
            | {
                  Bar = true
                  Baz = _
              } ->
                failwith "xxx"
            | _ -> None
"""

[<Test>]
let ``record destructuring in let binding`` () =
    formatSourceString
        false
        """
      module Foo =
          let someFunction { Firstname = fn; Lastname = ln; Age = age } =
              printfn "Name: %s" fn
              printfn "Last Name: %s" ln
              printfn "Age: %i" age
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let someFunction
        {
            Firstname = fn
            Lastname = ln
            Age = age
        }
        =
        printfn "Name: %s" fn
        printfn "Last Name: %s" ln
        printfn "Age: %i" age
"""

[<Test>]
let ``access modifier on short type record inside module`` () =
    formatSourceString
        false
        """
module Foo =
    type Stores =
        private {
            ModeratelyLongName : int
        }

    type private Bang = abstract Baz : int
"""
        { config with
            MaxLineLength = 40
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    type Stores =
        private
            {
                ModeratelyLongName : int
            }

    type private Bang =
        abstract Baz : int
"""

[<Test>]
let ``record with an access modifier and a static member`` () =
    formatSourceString
        false
        """
type RequestParser<'ctx, 'a> =
    internal
        { consumedFields: Set<ConsumedFieldName>
          parse: 'ctx -> Request -> Async<Result<'a, Error list>>
          prohibited: ProhibitedRequestGetter list }

        static member internal Create
            (
                consumedFields, parse: 'ctx -> Request -> Async<Result<'a, Error list>>
            ) : RequestParser<'ctx, 'a> =
            { consumedFields = consumedFields
              parse = parse
              prohibited = [] }

"""
        { config with
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type RequestParser<'ctx, 'a> =
    internal
        {
            consumedFields : Set<ConsumedFieldName>
            parse : 'ctx -> Request -> Async<Result<'a, Error list>>
            prohibited : ProhibitedRequestGetter list
        }

    static member internal Create
        (
            consumedFields,
            parse : 'ctx -> Request -> Async<Result<'a, Error list>>
        )
        : RequestParser<'ctx, 'a>
        =
        {
            consumedFields = consumedFields
            parse = parse
            prohibited = []
        }
"""

[<Test>]
let ``formatting error with MultilineBlockBracketsOnSameColumn, 1396`` () =
    formatSourceString
        false
        """
namespace GeeTower.Tests.EndToEnd

module WatcherTests =

    let CanRevokeAnIllegalCommitmentTx () =
        let lndAddress = obj()

        let config = {
            GeeTower.Backend.Configuration.GetTestingConfig (lndAddress.ToString())
            with
                BitcoinRpcUser = "btc"
        }

        ()
"""
        { config with
            MaxLineLength = 80
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
namespace GeeTower.Tests.EndToEnd

module WatcherTests =

    let CanRevokeAnIllegalCommitmentTx () =
        let lndAddress = obj ()

        let config =
            { GeeTower.Backend.Configuration.GetTestingConfig(
                  lndAddress.ToString()
              ) with
                BitcoinRpcUser = "btc"
            }

        ()
"""

[<Test>]
let ``a record type with accessibility modifier and members, 1824`` () =
    formatSourceString
        true
        """
namespace Thing

type Foo =
    private
        {
            Bar : int
            Qux : string
        }
    static member Baz : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Thing

type Foo =
    private
        {
            Bar : int
            Qux : string
        }

    static member Baz : int
"""

[<Test>]
let ``record type definition with members and trivia`` () =
    formatSourceString
        false
        """
type X = {
    Y : int
} with // foo
    member x.Z = ()
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type X =
    {
        Y : int
    } // foo
    member x.Z = ()
"""

[<Test>]
let ``anonymous records with comments on record fields`` () =
    formatSourceString
        false
        """
{|
    // The foo value.
    FooValue = fooValue
    // The bar value.
    BarValue = barValue
|}
"""
        config
    |> prepend newline
    |> should
        equal
        """
{|
    // The foo value.
    FooValue = fooValue
    // The bar value.
    BarValue = barValue
|}
"""

[<Test>]
let ``creating anonymous record based on a function call, 1749`` () =
    formatSourceString
        false
        """
type Foo = static member Create a = {| Name = "Isaac" |}
type Bar() = member _.Create (a,b) = ()
let bar = Bar()
type Thing = static member Stuff = 123
let x = {| Foo.Create ([ bar.Create(Thing.Stuff, Thing.Stuff) ]) with Age = 41 |}
"""
        { config with
            ArrayOrListMultilineFormatter = MultilineFormatterType.CharacterWidth
            SpaceBeforeUppercaseInvocation = true
            MaxArrayOrListWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
type Foo =
    static member Create a = {| Name = "Isaac" |}

type Bar() =
    member _.Create(a, b) = ()

let bar = Bar ()

type Thing =
    static member Stuff = 123

let x =
    {| Foo.Create (
           [
               bar.Create (Thing.Stuff, Thing.Stuff)
           ]
       ) with
        Age = 41
    |}
"""

[<Test>]
let ``comment after equals in record field`` () =
    formatSourceString
        false
        """
{ A = //comment 
      B }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{
    A = //comment
        B
}
"""

[<Test>]
let ``comment after equals in anonymous record field`` () =
    formatSourceString
        false
        """
{| A = //comment 
      B |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
{|
    A = //comment
        B
|}
"""

[<Test>]
let ``multiple base constructors in record`` () =
    formatSourceString
        false
        """
type UnhandledWebException =
    inherit Exception

    new(status: WebExceptionStatus, innerException: Exception) =
        { inherit Exception(SPrintF1
                                "Backend not prepared for this WebException with Status[%i]"
                                (int status),
                            innerException) }

    new(info: SerializationInfo, context: StreamingContext) =
        { inherit Exception(info, context) }
"""
        { config with MaxLineLength = 100 }
    |> prepend newline
    |> should
        equal
        """
type UnhandledWebException =
    inherit Exception

    new(status : WebExceptionStatus, innerException : Exception) =
        {
            inherit
                Exception(
                    SPrintF1
                        "Backend not prepared for this WebException with Status[%i]"
                        (int status),
                    innerException
                )
        }

    new(info : SerializationInfo, context : StreamingContext) = { inherit Exception(info, context) }
"""

[<Test>]
let ``inline anonymous record type declaration`` () =
    formatSourceString
        false
        """
type Foo =
    {
        Bar : {| X : string; Y : int; A : string; B : string |}
        Baz : int
        Blip : string
    }
"""
        config
    |> prepend newline

    |> should
        equal
        """
type Foo =
    {
        Bar :
            {|
                X : string
                Y : int
                A : string
                B : string
            |}
        Baz : int
        Blip : string
    }
"""

[<Test>]
let ``anonymous type alias`` () =
    formatSourceString
        false
        """
type A = {| x: int; y: AReallyLongTypeThatIsMuchLongerThan40Characters |}
"""
        config
    |> prepend newline

    |> should
        equal
        """
type A =
    {|
        x : int
        y : AReallyLongTypeThatIsMuchLongerThan40Characters
    |}
"""

[<Test>]
let ``inline anonymous type in function parameter`` () =
    formatSourceString
        false
        """
let f (x: {| x: int; y:  AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x
"""
        config
    |> prepend newline

    |> should
        equal
        """
let f
    (x :
        {|
            x : int
            y : AReallyLongTypeThatIsMuchLongerThan40Characters
        |})
    =
    x
"""

[<Test>]
let ``comment in bracket ranges of anonymous type, 2566`` () =
    formatSourceString
        false
        """
let x = {| // test1
    Y = 42
    Z = "string"
    Foo = "Bar"
    // test2
    |}

let y = {|
    Y = 42
    // test
|}

let z = {|
    Y = 42
|}

let a = {| // test1
    foo with
    Level = 7
    Square = 9
    // test2
|}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    {| // test1
        Y = 42
        Z = "string"
        Foo = "Bar"
    // test2
    |}

let y =
    {|
        Y = 42
    // test
    |}

let z = {| Y = 42 |}

let a =
    {| // test1
    foo with
        Level = 7
        Square = 9
    // test2
    |}
"""

[<Test>]
let ``equality comparison with a `with` expression should format correctly with Allman alignment, 2507`` () =
    formatSourceString
        false
        """
let compareThings (first: Thing) (second: Thing) =
    first = { second with
                Foo = first.Foo
                Bar = first.Bar
            }
"""
        { config with
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
let compareThings (first : Thing) (second : Thing) =
    first = { second with
                Foo = first.Foo
                Bar = first.Bar
            }
"""

// `Aligned` copy-and-update expression keeps label on first line to match G-Research style guide.
// See https://github.com/G-Research/fsharp-formatting-conventions#formatting-copy-and-update-record-expressions
[<Test>]
let ``update record in aligned style`` () =
    formatSourceString
        false
        """
// standalone
{ rainbow with Boss = "Jeffrey" ; Lackeys = [ "Zippy"; "George"; "Bungle" ] }

// binding expression
let v = { rainbow with Boss = "Jeffrey" ; Lackeys = [ "Zippy"; "George"; "Bungle" ] }
"""
        config
    |> prepend newline
    |> should
        equal
        """
// standalone
{ rainbow with
    Boss = "Jeffrey"
    Lackeys = [ "Zippy" ; "George" ; "Bungle" ]
}

// binding expression
let v =
    { rainbow with
        Boss = "Jeffrey"
        Lackeys = [ "Zippy" ; "George" ; "Bungle" ]
    }
"""

// In contrast, Stroustrup will indent the entire record body when the record is placed standalone.
[<Test>]
let ``update record in stroustrup style`` () =
    formatSourceString
        false
        """
// standalone
{ rainbow with Boss = "Jeffrey" ; Lackeys = [ "Zippy"; "George"; "Bungle" ] }

// binding expression
let v = { rainbow with Boss = "Jeffrey" ; Lackeys = [ "Zippy"; "George"; "Bungle" ] }
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
// standalone
{
    rainbow with
        Boss = "Jeffrey"
        Lackeys = [ "Zippy" ; "George" ; "Bungle" ]
}

// binding expression
let v = {
    rainbow with
        Boss = "Jeffrey"
        Lackeys = [ "Zippy" ; "George" ; "Bungle" ]
}
"""

[<Test>]
let ``anonymous struct record with trivia`` () =
    formatSourceString
        false
        """
struct // 1
    {| // 2
        // 3
        X = 4
    // 5       
    |} // 6 
"""
        config
    |> prepend newline
    |> should
        equal
        """
struct // 1
    {| // 2
        // 3
        X = 4
    // 5
    |} // 6
"""

[<Test>]
let ``multiline field body expression where indent_size = 2`` () =
    formatSourceString
        false
        """
let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
    let range =
      { Start =
          { Line = range.StartLine - 1
            Character = range.StartColumn }
        End =
          { Line = range.EndLine - 1
            Character = range.EndColumn } }

    [| { Range = range; NewText = formatted } |]
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let handlerFormattedRangeDoc (lines : NamedText, formatted : string, range : FormatSelectionRange) =
  let range =
    {
      Start =
        {
          Line = range.StartLine - 1
          Character = range.StartColumn
        }
      End =
        {
          Line = range.EndLine - 1
          Character = range.EndColumn
        }
    }

  [| { Range = range ; NewText = formatted } |]
"""
