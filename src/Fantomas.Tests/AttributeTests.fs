module Fantomas.Tests.AttributeTests

open NUnit.Framework
open FsUnit

open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep the attribute on top of the function``() =
    formatSourceString false """[<Extension>]
type Funcs = 
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))
    """ { config with MaxFunctionBindingWidth = 120 }
    |> should equal """[<Extension>]
type Funcs =
    [<Extension>]
    static member ToFunc(f: Action<_, _, _>) = Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c))
"""

[<Test>]
let ``attributes on expressions``() =
    formatSourceString false """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()
"""

[<Test>]
let ``attributes with multiple spaces between args on expressions``() =
    formatSourceString false """
    [<Dependency         ("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()
"""

[<Test>]
let ``attributes without parentheses on expressions``() =
    formatSourceString false """
    [<MyValue 55>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<MyValue 55>]
do ()
"""

[<Test>]
let ``attributes without parentheses and multiples spaces between args on expressions``() =
    formatSourceString false """
    [<MyValue       55>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<MyValue 55>]
do ()
"""

[<Test>]
let ``units of measures declaration``() =
    formatSourceString false """
    [<Measure>] type m
    [<Measure>] type kg
    [<Measure>] type s
    [<Measure>] type N = kg m / s^2
    [<Measure>] type Pa = N * m^2""" config
    |> prepend newline
    |> should equal """
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
let ``type params``() =
    formatSourceString false """
let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y
type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}""" config
    |> prepend newline
    |> should equal """
let genericSumUnits (x: float<'u>) (y: float<'u>) = x + y

type vector3D<[<Measure>] 'u> =
    { x: float<'u>
      y: float<'u>
      z: float<'u> }
"""

[<Test>]
let ``attributes on recursive functions``() =
    formatSourceString false """
let rec [<Test>] a () = 10
and [<Test>] b () = 10""" config
    |> prepend newline
    |> should equal """
[<Test>]
let rec a () = 10

and [<Test>] b () = 10
"""

[<Test>]
let ``attributes on implicit constructors``() =
    formatSourceString false """
[<Export>]
type Sample [<ImportingConstructor>] (dependency: IDependency) = class end
[<Export>]
type Sample [<ImportingConstructor>] internal () = class end""" config
    |> prepend newline
    |> should equal """
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
let ``should handle targets on attributes``() =
    formatSourceString false """
[<DataContract>]
type Foo = 
    { [<field:DataMember>]
      Bar:string }
"""  config
  |> prepend newline
  |> should equal """
[<DataContract>]
type Foo =
    { [<field:DataMember>]
      Bar: string }
"""

[<Test>]
let ``print trivia linked to SynAttribute`` () =
    let source = """
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
    |> should equal """module MyApp

#if DEBUG
[<Emit("console.log('%c' +  $1, 'color: ' + $0)")>]
let printInColor (color: string) (msg: string): unit = jsNative

[<Emit("console.log('%c' +  $1, $0)")>]
let printInStyle (style: string) (msg): unit = jsNative

[<Emit("console.info($0)")>]
let printModel model: unit = jsNative

[<Emit("console.trace()")>]
let printStackTrace (): unit = jsNative
#endif

let e2e value = Props.Data("e2e", value)
"""

[<Test>]
let ``comments before attributes should be added correctly, issue 422`` () =
    formatSourceString false """module RecordTypes = 

    /// Records can also be represented as structs via the 'Struct' attribute.
    /// This is helpful in situations where the performance of structs outweighs
    /// the flexibility of reference types.
    [<Struct>]
    type ContactCardStruct = 
        { Name     : string
          Phone    : string
          Verified : bool }
"""  config
    |> should equal """module RecordTypes =

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
    formatSourceString false """    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    let foo = ()"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceStringWithDefines [] """    [<
#if NETCOREAPP2_1
      Builder.Object;
#else
      Widget;
#endif
      DefaultValue(true)>]
    let foo = ()"""  config
    |> prepend newline
    |> should equal """
[<
#if NETCOREAPP2_1

#else
  Widget;
#endif
  DefaultValue(true)>]
let foo = ()
"""

[<Test>]
let ``attribute above extern keyword, 562`` () =
    formatSourceString false """
module C =
  [<DllImport("")>]
  extern IntPtr f()
"""  ({ config with StrictMode = true })
    |> prepend newline
    |> should equal """
module C =
    [<DllImport("")>]
    extern IntPtr f()
"""

[<Test>]
let ``keep single newline between attribute and let binding, 611`` () =
    formatSourceString false """
open System
open Library

[<EntryPoint>]

let main argv =
    printfn "Nice command-line arguments! Here's what JSON.NET has to say about them:" argv
    |> Array.map getJsonNetJson |> Array.iter (printfn "%s")
    0 // return an integer exit code
"""  ({ config with
            SpaceAfterComma = false
            SpaceAfterSemicolon = false
            SpaceAroundDelimiter = false
            SpaceBeforeLowercaseInvocation = false })
    |> prepend newline
    |> should equal """
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
    formatSourceString false """namespace Foo.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: AssemblyTitle("Foo")>]
[<assembly: AssemblyDescription("")>]

do
  ()
"""  config
    |> prepend newline
    |> should equal """
namespace Foo.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly:AssemblyTitle("Foo")>]
[<assembly:AssemblyDescription("")>]

do ()
"""

[<Test>]
let ``should preserve single return type attribute`` () =
    formatSourceString false """let f x : [<return: Attribute>] int = x""" config
    |> should equal """let f x: [<return:Attribute>] int = x
"""

[<Test>]
let ``should preserve multiple return type attributes`` () =
    formatSourceString false """let f x : [<return: AttributeOne;AttributeTwo;AttributeThree("foo")>] int = x""" config
    |> should equal """let f x: [<return:AttributeOne; AttributeTwo; AttributeThree("foo")>] int = x
"""

[<Test>]
let ``attribute, new line, let binding`` () =
    formatSourceString false """
    [<Foo>]

let bar = 7
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]

let bar = 7
"""

[<Test>]
let ``attribute, new line, type declaration`` () =
    formatSourceString false """
[<Foo>]

type Bar = Bar of string
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]

type Bar = Bar of string
"""

[<Test>]
let ``attribute, new line, attribute, newline, let binding`` () =
    formatSourceString false """
[<Foo>]

[<Meh>]

let bar = 7
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]

[<Meh>]

let bar = 7
"""

[<Test>]
let ``attribute, new line, attribute, line comment, type declaration`` () =
    formatSourceString false """
[<Foo>]

[<Meh>]
// foo
type Text = string
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]

[<Meh>]
// foo
type Text = string
"""

[<Test>]
let ``attribute, hash directive, attribute, hash directive, type declaration`` () =
    formatSourceString false """
[<Foo>]
#if FOO
[<Meh>]
#endif
type Text = string
"""  config
    |> prepend newline
    |> should equal """
[<Foo>]
#if FOO
[<Meh>]
#endif
type Text = string
"""

[<Test>]
let ``attribute, line comment, attribute, new line, record definition field`` () =
    formatSourceString false """
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]
      
      DisplayName: string }
"""  config
    |> prepend newline
    |> should equal """
type Commenter =
    { [<JsonProperty("display_name")>]
      // foo
      [<Bar>]

      DisplayName: string }
"""
