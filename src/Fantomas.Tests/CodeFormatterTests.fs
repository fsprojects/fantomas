module Fantomas.Tests.CodeFormatterTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``module abbreviation``() =
    formatSourceString "module ES = Microsoft.FSharp.Quotations.ExprShape" config
    |> should equal "module ES = Microsoft.FSharp.Quotations.ExprShape"

[<Test>]
let ``attributes on expressions``() =
    formatSourceString """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()"""

[<Test>]
let ``module with functions``() =
    formatSourceString "module internal MyModule = let x = 42" config
    |> prepend newline
    |> should equal """
module internal MyModule = 
    let x = 42"""

[<Test>]
let ``open modules``() =
    formatSourceString """
    open System
    open System.IO""" config
    |> prepend newline
    |> should equal """
open System
open System.IO"""

[<Test>]
let ``recursive functions``() =
    formatSourceString """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x
and g x = x"""

[<Test>]
let ``hash directives``() =
    formatSourceString """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """ config
    |> prepend newline
    |> append newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``discriminated unions declaration``() =
    formatSourceString "type X = private | A of AParameters | B" config
    |> prepend newline
    |> should equal """
type X = 
    private
    | A of AParameters
    | B
"""

[<Test>]
let ``record declaration``() =
    formatSourceString "type AParameters = { a : int }" config
    |> prepend newline
    |> should equal """
type AParameters = 
    { a : int }
"""

[<Test>]
let ``enums declaration``() =
    formatSourceString """
    type FontVariant =
    | [<Description("small-caps")>] SmallCaps = 0""" config
    |> prepend newline
    |> should equal """
type FontVariant = 
    | [<Description("small-caps")>] SmallCaps = 0
"""

[<Test>]
let ``units of measures``() =
    formatSourceString """
    [<Measure>] type m
    [<Measure>] type kg
    [<Measure>] type s
    [<Measure>] type N = kg m / s^2
    [<Measure>] type Pa = N / m^2
    let a = 1.<m/Pa*s>""" config
    |> prepend newline
    |> should equal """
[<Measure>]
type m

[<Measure>]
type kg

[<Measure>]
type s

[<Measure>]
type N = kg m * s^2

[<Measure>]
type Pa = N * m^2

let a = 1.0<m/Pa*s>"""

