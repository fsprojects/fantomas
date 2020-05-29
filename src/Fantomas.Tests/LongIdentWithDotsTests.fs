module Fantomas.Tests.LongIdentWithDotsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``Fluent api should not remain on the same lines``() =
    formatSourceString false """
Log.Logger <- 
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .CreateLogger()""" config
    |> prepend newline
    |> should equal """
Log.Logger <- LoggerConfiguration().Destructure.FSharpTypes().WriteTo.Console().CreateLogger()
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

[<Test>]
let ``method call on multiple lines`` () =
    formatSourceString false """module Program

[<EntryPoint>]
let main _ =
    try
        try
            Config.Logger.configure ()

            let config =
                ConfigurationBuilder()
                    .SetBasePath(Directory.GetCurrentDirectory())
                    .Build()

            WebHostBuilder()
                .UseConfiguration(config)
                .UseKestrel()
                .UseSerilog()
                .ConfigureAppConfiguration(Action<WebHostBuilderContext, IConfigurationBuilder> configureAppConfiguration)
                .ConfigureServices(Action<WebHostBuilderContext, IServiceCollection> configureServices)
                .Configure(Action<IApplicationBuilder> configureApp)
                .Build()
                .Run()
                |> ignore

            0
        with
        | ex ->
            Log.Fatal(ex, "Service terminated unexpectedly")

            1
    finally
        Log.CloseAndFlush()
"""  config
    |> prepend newline
    |> should equal """
module Program

[<EntryPoint>]
let main _ =
    try
        try
            Config.Logger.configure ()

            let config =
                ConfigurationBuilder().SetBasePath(Directory.GetCurrentDirectory()).Build()

            WebHostBuilder().UseConfiguration(config).UseKestrel().UseSerilog()
                .ConfigureAppConfiguration
                    (Action<WebHostBuilderContext, IConfigurationBuilder> configureAppConfiguration)
                .ConfigureServices(Action<WebHostBuilderContext, IServiceCollection> configureServices)
                .Configure(Action<IApplicationBuilder> configureApp).Build().Run()
            |> ignore

            0
        with ex ->
            Log.Fatal(ex, "Service terminated unexpectedly")

            1
    finally
        Log.CloseAndFlush()
"""

[<Test>]
let ``chained lambda should start on same line as dot, 871`` () =
    formatSourceString false """namespace LoginWithBulmaTutorial

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Notation
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type MySPA = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        let passwordValid = Var.Create true
        let emailValid = Var.Create true

        MySPA()
            .AttrPassword(Attr.ClassPred "is-danger" (not passwordValid.V))
            .AttrEmail(Attr.ClassPred "is-danger" (not emailValid.V))
            .Login(fun e ->
                    passwordValid
                    := not (String.IsNullOrWhiteSpace e.Vars.Password.Value)
                    emailValid
                    := not (String.IsNullOrWhiteSpace e.Vars.Email.Value)

                    if passwordValid.Value && emailValid.Value
                    then JS.Alert(sprintf "Your email is %s" e.Vars.Email.Value)
                    e.Event.PreventDefault()
            )
            .Bind()
"""  config
    |> prepend newline
    |> should equal """
namespace LoginWithBulmaTutorial

open System
open WebSharper
open WebSharper.JavaScript
open WebSharper.JQuery
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Notation
open WebSharper.UI.Templating

[<JavaScript>]
module Client =
    // The templates are loaded from the DOM, so you just can edit index.html
    // and refresh your browser, no need to recompile unless you add or remove holes.
    type MySPA = Template<"wwwroot/index.html", ClientLoad.FromDocument>

    [<SPAEntryPoint>]
    let Main () =
        let passwordValid = Var.Create true
        let emailValid = Var.Create true

        MySPA().AttrPassword(Attr.ClassPred "is-danger" (not passwordValid.V))
            .AttrEmail(Attr.ClassPred "is-danger" (not emailValid.V))
            .Login(fun e ->
                  passwordValid
                  := not (String.IsNullOrWhiteSpace e.Vars.Password.Value)
                  emailValid
                  := not (String.IsNullOrWhiteSpace e.Vars.Email.Value)

                  if passwordValid.Value && emailValid.Value
                  then JS.Alert(sprintf "Your email is %s" e.Vars.Email.Value)
                  e.Event.PreventDefault()).Bind()
"""
