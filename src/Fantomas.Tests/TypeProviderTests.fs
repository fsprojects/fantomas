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
type Northwind = ODataService<"http://services.odata.org/Northwind/Northwind.svc/">
"""