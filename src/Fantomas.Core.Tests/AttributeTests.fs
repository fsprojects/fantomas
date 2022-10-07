module Fantomas.Core.Tests.AttributeTests

open NUnit.Framework
open FsUnit

open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``should keep the attribute on top of the function`` () =
    formatSourceString
        false
        """[<Extension>]
type Funcs =
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))
    """
        { config with MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """[<Extension>]
type Funcs =
    [<Extension>]
    static member ToFunc(f: Action<_, _, _>) = Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c))
"""

[<Test>]
let ``attributes on expressions`` () =
    formatSourceString
        false
        """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()
"""

[<Test>]
let ``attributes with multiple spaces between args on expressions`` () =
    formatSourceString
        false
        """
    [<Dependency         ("FSharp.Compiler", LoadHint.Always)>]
    do ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()
"""

[<Test>]
let ``attributes without parentheses on expressions`` () =
    formatSourceString
        false
        """
    [<MyValue 55>]
    do ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<MyValue 55>]
do ()
"""

[<Test>]
let ``attributes without parentheses and multiples spaces between args on expressions`` () =
    formatSourceString
        false
        """
    [<MyValue       55>]
    do ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<MyValue 55>]
do ()
"""

[<Test>]
let ``units of measures declaration`` () =
    formatSourceString
        false
        """
    [<Measure>] type m
    [<Measure>] type kg
    [<Measure>] type s
    [<Measure>] type N = kg m / s^2
    [<Measure>] type Pa = N * m^2"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Measure>]
type m

[<Measure>]
type kg

[<Measure>]
type s

[<Measure>]
type N = kg m / s^2

[<Measure>]
type Pa = N * m^2
"""

[<Test>]
let ``type params`` () =
    formatSourceString
        false
        """
let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y
type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}"""
        config
    |> prepend newline
    |> should
        equal
        """
let genericSumUnits (x: float<'u>) (y: float<'u>) = x + y

type vector3D<[<Measure>] 'u> =
    { x: float<'u>
      y: float<'u>
      z: float<'u> }
"""

[<Test>]
let ``attributes on recursive functions`` () =
    formatSourceString
        false
        """
let rec [<Test>] a () = 10
and [<Test>] b () = 10"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Test>]
let rec a () = 10

and [<Test>] b () = 10
"""

[<Test>]
let ``attributes on implicit constructors`` () =
    formatSourceString
        false
        """
[<Export>]
type Sample [<ImportingConstructor>] (dependency: IDependency) = class end
[<Export>]
type Sample [<ImportingConstructor>] internal () = class end"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Export>]
type Sample [<ImportingConstructor>] (dependency: IDependency) =
    class
    end

[<Export>]
type Sample [<ImportingConstructor>] internal () =
    class
    end
"""

[<Test>]
let ``should handle targets on attributes`` () =
    formatSourceString
        false
        """
[<DataContract>]
type Foo =
    { [<field:DataMember>]
      Bar:string }
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<DataContract>]
type Foo =
    { [<field: DataMember>]
      Bar: string }
"""

[<Test>]
let ``print trivia linked to SynAttribute`` () =
    let source =
        """
module MyApp

#if DEBUG
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color:string) (msg:string):unit = jsNative

[<Emit("console.log('%c' +  $1, $0)")>]
let printInStyle (style:string) (msg): unit = jsNative

[<Emit("console.info($0)")>]
let printModel model : unit = jsNative

[<Emit("console.trace()")>]
let printStackTrace (): unit = jsNative
#endif

let e2e value =
    Props.Data("e2e", value)
"""

    formatSourceString false source config
    |> should
        equal
        """module MyApp

#if DEBUG
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color: string) (msg: string) : unit = jsNative

[<Emit("console.log('%c' +  $1, $0)")>]
let printInStyle (style: string) (msg) : unit = jsNative

[<Emit("console.info($0)")>]
let printModel model : unit = jsNative

[<Emit("console.trace()")>]
let printStackTrace () : unit = jsNative
#endif

let e2e value = Props.Data("e2e", value)
"""

[<Test>]
let ``comments before attributes should be added correctly, issue 422`` () =
    formatSourceString
        false
        """module RecordTypes =

    /// Records can also be represented as structs via the 'Struct' attribute.
    /// This is helpful in situations where the performance of structs outweighs
    /// the flexibility of reference types.
    [<Struct>]
    type ContactCardStruct =
        { Name     : string
          Phone    : string
          Verified : bool }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module RecordTypes =

    /// Records can also be represented as structs via the 'Struct' attribute.
    /// This is helpful in situations where the performance of structs outweighs
    /// the flexibility of reference types.
    [<Struct>]
    type ContactCardStruct =
        { Name: string
          Phone: string
          Verified: bool }
"""

[<Test>]
let ``different attributes according to defines`` () =
    formatSourceString
        false
        """    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    let foo = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<
#if NETCOREAPP2_1
  Builder.Object;
#else
  Widget;
#endif
  DefaultValue(true)>]
let foo = ()
"""

[<Test>]
let ``different attributes according to defines, no defines`` () =
    formatSourceStringWithDefines
        []
        """    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    let foo = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<
#if NETCOREAPP2_1
#else
  Widget;
#endif
  DefaultValue(true)>]
let foo = ()
"""

[<Test>]
let ``different attributes according to defines, NETCOREAPP2_1`` () =
    formatSourceStringWithDefines
        [ "NETCOREAPP2_1" ]
        """    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    let foo = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
[<
#if NETCOREAPP2_1
  Builder.Object;
#else
#endif
  DefaultValue(true)>]
let foo = ()
"""

[<Test>]
let ``keep single newline between attribute and let binding, 611`` () =
    formatSourceString
        false
        """
open System
open Library

[<EntryPoint>]

let main argv =
    printfn "Nice command-line arguments! Here's what JSON.NET has to say about them:" argv
    |> Array.map getJsonNetJson |> Array.iter (printfn "%s")
    0 // return an integer exit code
"""
        { config with
            SpaceAfterComma = false
            SpaceAfterSemicolon = false
            SpaceAroundDelimiter = false
            SpaceBeforeLowercaseInvocation = false }
    |> prepend newline
    |> should
        equal
        """
open System
open Library

[<EntryPoint>]

let main argv =
    printfn "Nice command-line arguments! Here's what JSON.NET has to say about them:" argv
    |> Array.map getJsonNetJson
    |> Array.iter(printfn "%s")

    0 // return an integer exit code
"""

[<Test>]
let ``multiple assembly attributes, 796`` () =
    formatSourceString
        false
        """namespace Foo.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: AssemblyTitle("Foo")>]
[<assembly: AssemblyDescription("")>]

do
  ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: AssemblyTitle("Foo")>]
[<assembly: AssemblyDescription("")>]

do ()
"""

[<Test>]
let ``should preserve single return type attribute`` () =
    formatSourceString false """let f x : [<return: Attribute>] int = x""" config
    |> should
        equal
        """let f x : [<return: Attribute>] int = x
"""

[<Test>]
let ``should preserve multiple return type attributes`` () =
    formatSourceString false """let f x : [<return: AttributeOne;AttributeTwo;AttributeThree("foo")>] int = x""" config
    |> should
        equal
        """let f x : [<return: AttributeOne; AttributeTwo; AttributeThree("foo")>] int = x
"""

[<Test>]
let ``attribute, new line, let binding`` () =
    formatSourceString
        false
        """
    [<Foo>]

let bar = 7
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]

let bar = 7
"""

[<Test>]
let ``attribute, new line, type declaration`` () =
    formatSourceString
        false
        """
[<Foo>]

type Bar = Bar of string
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]

type Bar = Bar of string
"""

[<Test>]
let ``attribute, new line, attribute, newline, let binding`` () =
    formatSourceString
        false
        """
[<Foo>]

[<Meh>]

let bar = 7
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]

[<Meh>]

let bar = 7
"""

[<Test>]
let ``attribute, new line, attribute, line comment, type declaration`` () =
    formatSourceString
        false
        """
[<Foo>]

[<Meh>]
// foo
type Text = string
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]

[<Meh>]
// foo
type Text = string
"""

[<Test>]
let ``attribute, hash directive, attribute, hash directive, type declaration`` () =
    formatSourceString
        false
        """
[<Foo>]
#if FOO
[<Meh>]
#endif
type Text = string
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]
#if FOO
[<Meh>]
#endif
type Text = string
"""

[<Test>]
let ``attribute, line comment, attribute, new line, record definition field`` () =
    formatSourceString
        false
        """
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]

      DisplayName: string }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]

      DisplayName: string }
"""

[<Test>]
let ``assembly attributes remain on own line, 629`` () =
    formatSourceString
        false
        """
namespace AltCover.Visualizer

open System
open System.Reflection
open System.Runtime.InteropServices

[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
[<assembly:AssemblyTitle("AltCover.Visualizer")>]
[<assembly:AssemblyDescription("Coverage and static analysis visualizer for NCover (possibly extended) and OpenCover")>]
[<assembly:System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
()
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace AltCover.Visualizer

open System
open System.Reflection
open System.Runtime.InteropServices

[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
[<assembly: AssemblyTitle("AltCover.Visualizer")>]
[<assembly: AssemblyDescription("Coverage and static analysis visualizer for NCover (possibly extended) and OpenCover")>]
[<assembly: System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
()
"""

[<Test>]
let ``line comment between attributes and do expression`` () =
    formatSourceString
        false
        """
[<Foo>]
[<Bar>]
// barry
printfn "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]
[<Bar>]
// barry
printfn "meh"
"""

[<Test>]
let ``multiple attributes inside SynAttributes that exceeds max line length, 629`` () =
    formatSourceString
        false
        """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
  inherit Controller()

  [<HttpGet("jobs/all");
    ProducesResponseType(typeof<bool>, 200);
    ProducesResponseType(404);
    Authorize(AuthorizationScopePolicies.Read)>]
  member _.ListJobs(): Task<UserCmdResponseMsg> =
    task {
      return!
        akkaService.ImporterSystem.ApiMaster <? ApiMasterMsg.GetAllJobsCmd
    }

  [<HttpPost("jobs/create");
    DisableRequestSizeLimit;
    RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
    ProducesResponseType(typeof<RoleChangeSummaryDto list>, 200);
    ProducesResponseType(404);
    Authorize(AuthorizationScopePolicies.Write)>]
  member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
    let importer = akkaService.ImporterSystem

    ActionResult.ofAsyncResult <| asyncResult {
      let! state =
        (LowerCaseString.create args.State, file)
        |> pipeObjectThroughValidation [ (fst, [stateIsValid]); (snd, [(fun s -> Ok s)]) ]

      let! filePath = FormFile.downloadAsTemp file

      let job =
        { JobType = EsriBoundaryImport
          FileToImport = filePath
          State = state
          DryRun = args.DryRun }

      importer.ApiMaster <! StartImportCmd job
      return Ok job
    }
"""
        { config with
            MaxInfixOperatorExpression = 40
            MaxArrayOrListWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
//[<ApiExplorerSettings(IgnoreApi = true)>]
[<Route("api/v1/admin/import")>]
type RoleAdminImportController(akkaService: AkkaService) =
    inherit Controller()

    [<HttpGet("jobs/all");
      ProducesResponseType(typeof<bool>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Read)>]
    member _.ListJobs() : Task<UserCmdResponseMsg> =
        task {
            return!
                akkaService.ImporterSystem.ApiMaster
                <? ApiMasterMsg.GetAllJobsCmd
        }

    [<HttpPost("jobs/create");
      DisableRequestSizeLimit;
      RequestFormLimits(MultipartBodyLengthLimit = 509715200L);
      ProducesResponseType(typeof<RoleChangeSummaryDto list>, 200);
      ProducesResponseType(404);
      Authorize(AuthorizationScopePolicies.Write)>]
    member _.StartJob(file: IFormFile, [<FromQuery>] args: ImporterJobArgs) =
        let importer = akkaService.ImporterSystem

        ActionResult.ofAsyncResult
        <| asyncResult {
            let! state =
                (LowerCaseString.create args.State, file)
                |> pipeObjectThroughValidation
                    [ (fst, [ stateIsValid ])
                      (snd, [ (fun s -> Ok s) ]) ]

            let! filePath = FormFile.downloadAsTemp file

            let job =
                { JobType = EsriBoundaryImport
                  FileToImport = filePath
                  State = state
                  DryRun = args.DryRun }

            importer.ApiMaster <! StartImportCmd job
            return Ok job
        }
"""

[<Test>]
let ``compiler defines around SynAttribute nodes, 631`` () =
    formatSourceString
        false
        """
type internal Handler() =
  class
    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
    DefaultValue(true)>]
    val mutable mainWindow: Window
end

"""
        config
    |> prepend newline
    |> should
        equal
        """
type internal Handler() =
    class
        [<
#if NETCOREAPP2_1
          Builder.Object;
#else
          Widget;
#endif
          DefaultValue(true)>]
        val mutable mainWindow: Window
    end
"""

[<Test>]
let ``attribute on member of recursive type, 1918`` () =
    formatSourceString
        false
        """
type X = A
and Y = B
    with
        [<ExcludeFromCodeCoverage>]
        member  this.M() = true
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type X = A

and Y = B
    with
        [<ExcludeFromCodeCoverage>]
        member this.M() = true
"""

[<Test>]
let ``attribute on second member defn, 1898`` () =
    formatSourceString
        false
        """
type Test1() =
  member x.Test() = ()

and Test2() =

  let someEvent = Event<EventHandler<int>, int>()

  [<CLIEvent>]
  member x.SomeEvent = someEvent.Publish
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Test1() =
    member x.Test() = ()

and Test2() =

    let someEvent = Event<EventHandler<int>, int>()

    [<CLIEvent>]
    member x.SomeEvent = someEvent.Publish
"""

[<Test>]
let ``attributes on recursive discriminated union types, 1874`` () =
    formatSourceString
        false
        """
module test
open System.Diagnostics

type Correct =
    | A of unit

    [<DebuggerStepThrough>]
    override this.ToString () = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()

and Wrong =
    | B of unit

    [<DebuggerStepThrough>]
    override this.ToString () = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module test

open System.Diagnostics

type Correct =
    | A of unit

    [<DebuggerStepThrough>]
    override this.ToString() = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()

and Wrong =
    | B of unit

    [<DebuggerStepThrough>]
    override this.ToString() = ""

    [<DebuggerStepThrough>]
    member this.f = ()

    [<DebuggerStepThrough>]
    static member this.f = ()
"""

[<Test>]
let ``attribute on member of recursive record type, 1962`` () =
    formatSourceString
        false
        """
module Foo =

    type Person = {
        Name : string
        FavoriteDog : Dog
    } with
        [<RequiresExplicitTypeArguments>]
        static member doThing person =
            ()
    and Dog = {
        Name : string
        FavoriteChewToy : string
    } with
        [<RequiresExplicitTypeArguments>]
        static member doThing person =
            ()
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
module Foo =

    type Person =
        { Name: string
          FavoriteDog: Dog }
        [<RequiresExplicitTypeArguments>]
        static member doThing person = ()

    and Dog =
        { Name: string
          FavoriteChewToy: string }
        [<RequiresExplicitTypeArguments>]
        static member doThing person = ()
"""

[<Test>]
let ``comment after attribute before let binding with return type`` () =
    formatSourceString
        false
        """
[<Foo>]
// bar
let add (a:  int) (  b : int) : int = a + b
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]
// bar
let add (a: int) (b: int) : int = a + b
"""

[<Test>]
let ``comment after attribute before value binding with return type`` () =
    formatSourceString
        false
        """
[<Foo>]
// bar

// bar again, cuz why not
let x: int = 99
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]
// bar

// bar again, cuz why not
let x: int = 99
"""

[<Test>]
let ``comment between attribute and nested module, 2016`` () =
    formatSourceString
        false
        """
[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncOptionCEExtensions =

    type AsyncOptionBuilder with
        member inline __.Source(s: #seq<_>) = s
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncOptionCEExtensions =

    type AsyncOptionBuilder with
        member inline __.Source(s: #seq<_>) = s
"""

[<Test>]
let ``comment between attribute and nested module, signature file`` () =
    formatSourceString
        true
        """
[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncOptionCEExtensions =

    type AsyncOptionBuilder with
        member inline Source : string -> string

"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
[<AutoOpen>]
// Having members as extensions gives them lower priority in
// overload resolution and allows skipping more type annotations.
module AsyncOptionCEExtensions =

    type AsyncOptionBuilder with
        member inline Source: string -> string
"""

[<Test>]
let ``comment between attribute and member, 2130`` () =
    formatSourceString
        false
        """
type StreamReaderExtensions = 
  [<Extension>]
  // Extension method for StreamReader using Tasks to enable specifying timeout.
  // Returns `None` if timeout occurs, otherwise `Some string`.
  // Beware that since StreamReader.ReadLineAsync task can't be cancelled,
  // it might still be running after recieving None!
  static member inline Meh(streamReader: StreamReader, timeout: TimeSpan) =
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type StreamReaderExtensions =
    [<Extension>]
    // Extension method for StreamReader using Tasks to enable specifying timeout.
    // Returns `None` if timeout occurs, otherwise `Some string`.
    // Beware that since StreamReader.ReadLineAsync task can't be cancelled,
    // it might still be running after recieving None!
    static member inline Meh(streamReader: StreamReader, timeout: TimeSpan) = ()
"""

[<Test>]
let ``trivia in nested multiline tuple expression in attribute, 2525`` () =
    formatSourceString
        false
        """
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member",  // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<assembly: SuppressMessage("Gendarme.Rules.Performance",
                            "AvoidRepetitiveCallsToPropertiesRule",
                            Scope = "member", // MethodDefinition
                            Target = "AltCover.Recorder.Instance/I/CallTrack::instance()",
                            Justification = "Bytecode delta only")>]
()
"""
