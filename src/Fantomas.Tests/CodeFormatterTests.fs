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
    formatString "module ES = Microsoft.FSharp.Quotations.ExprShape" config
    |> should equal "module ES = Microsoft.FSharp.Quotations.ExprShape"

[<Test>]
let ``attributes on expressions``() =
    formatString """
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> prepend newline
    |> should equal """
[<Dependency("FSharp.Compiler", LoadHint.Always)>]
do ()"""

[<Test>]
let ``module with functions``() =
    formatString "module internal MyModule = let x = 42" config
    |> prepend newline
    |> should equal """
module internal MyModule = 
    let x = 42"""

[<Test>]
let ``open modules``() =
    formatString """
    open System
    open System.IO""" config
    |> prepend "\r\n"
    |> should equal """
open System
open System.IO"""

[<Test>]
let ``recursive functions``() =
    formatString """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x
and g x = x"""

[<Test>]
let ``hash directives``() =
    formatString """
    #r "Fantomas.Tests.dll"
    #load "Tests.fs"
    """ config
    |> prepend newline
    |> append newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "Tests.fs"
"""

