module Fantomas.Tests.AttributeTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
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
    static member ToFunc(f : Action<_, _, _>) = 
        Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c))
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
let genericSumUnits (x : float<'u>) (y : float<'u>) = x + y

type vector3D<[<Measure>] 'u> = 
    { x : float<'u>
      y : float<'u>
      z : float<'u> }
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
