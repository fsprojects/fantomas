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
    """ config
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
let rec a() = 10

and [<Test>] b() = 10
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
let printStackTrace(): unit = jsNative
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
    |> should equal """#if NETCOREAPP2_1
[<Builder.Object>]
#else
[<Widget>]
#endif
[<DefaultValue(true)>]
let foo = ()
"""