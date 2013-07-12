module Fantomas.Tests.TypeProviderTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``type providers``() =
    formatSourceString false """
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">""" config
    |> prepend newline
    |> should equal """
type Northwind = ODataService< "http://services.odata.org/Northwind/Northwind.svc/" >
"""

[<Test>]
let ``should add space before type provider params``() =
    formatSourceString false """
type IntegerRegex = FSharpx.Regex< @"(?<value>\d+)" >""" config
    |> prepend newline
    |> should equal """
type IntegerRegex = FSharpx.Regex< @"(?<value>\d+)" >
"""