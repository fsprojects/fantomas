module Fantomas.Tests.MaxChainedExpressionWidth

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``align chained expression if threshold was reached`` () =
    formatSourceString false """
let result = CaptureStartupErrors(true).UseSerilog(dispose = true).UseStartup()
"""  config
    |> prepend newline
    |> should equal """
let result =
    CaptureStartupErrors(true)
        .UseSerilog(dispose = true)
        .UseStartup()
"""

[<Test>]
let ``generic parameter in chained expression`` () =
    formatSourceString false """
builder.CaptureStartupErrors(true).UseSerilog(dispose = true).UseStartup<Startup>()
"""  config
    |> prepend newline
    |> should equal """
builder
    .CaptureStartupErrors(true)
    .UseSerilog(dispose = true)
    .UseStartup<Startup>()
"""

[<Test>]
let ``aspnet core webhost builder sample`` () =
    formatSourceString false """
module Program

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Serilog
open Startup

[<EntryPoint>]
let main args =
  Host
    .CreateDefaultBuilder(args)
    .ConfigureWebHostDefaults(fun builder ->
      builder
        .CaptureStartupErrors(true)
        .UseSerilog(dispose = true)
        .UseStartup<Startup>()
      |> ignore
    )
    .Build()
    .Run()
  0
"""  config
    |> prepend newline
    |> should equal """
module Program

open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Serilog
open Startup

[<EntryPoint>]
let main args =
    Host
        .CreateDefaultBuilder(args)
        .ConfigureWebHostDefaults(fun builder ->
        builder
            .CaptureStartupErrors(true)
            .UseSerilog(dispose = true)
            .UseStartup<Startup>()
        |> ignore)
        .Build()
        .Run()
    0
"""

[<Test>]
let ``short chained expression stays in one line`` () =
    formatSourceString false """
let foo = Host
             .CreateDefaultBuilder(args)
"""  config
    |> prepend newline
    |> should equal """
let foo = Host.CreateDefaultBuilder(args)
"""
