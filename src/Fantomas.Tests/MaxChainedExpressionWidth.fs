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