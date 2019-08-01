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

[<Test>]
let ``fluent api with comments should remain on same lines`` () =
    formatSourceString false """
Log.Logger <- 
  LoggerConfiguration() 
   // Suave.SerilogExtensions has native destructuring mechanism
   // this helps Serilog deserialize the fsharp types like unions/records
   .Destructure.FSharpTypes()
   // use package Serilog.Sinks.Console  
   // https://github.com/serilog/serilog-sinks-console
   .WriteTo.Console() 
   // add more sinks etc.
   .CreateLogger()""" config
    |> prepend newline
    |> should equal """
Log.Logger <-
    LoggerConfiguration()
        // Suave.SerilogExtensions has native destructuring mechanism
        // this helps Serilog deserialize the fsharp types like unions/records
        .Destructure.FSharpTypes()
        // use package Serilog.Sinks.Console
        // https://github.com/serilog/serilog-sinks-console
        .WriteTo.Console()
        // add more sinks etc.
        .CreateLogger()
"""


[<Test>]
let ``force newline by adding comments`` () =
    formatSourceString false """let config = //
    Builder()//
        .UseCaching()//
        .UseSql()//
        .UseMeh()
"""  config
    |> should equal """let config = //
    Builder() //
        .UseCaching() //
        .UseSql() //
        .UseMeh()
"""