module Fantomas.Tests.LongIdentWithDotsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``Fluent api should remain on the same lines``() =
    formatSourceString false """
Log.Logger <- 
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .CreateLogger()""" config
    |> prepend newline
    |> should equal """
Log.Logger <-
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .CreateLogger()
"""