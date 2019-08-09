module Fantomas.Tests.KeepNewlineAfterTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let configKNA = { config with KeepNewlineAfter = true }

[<Test>]
let ``after equal in let binding`` () =
    formatSourceString false """let a =
printfn "foo: %d"   42
"""  configKNA
    |> prepend newline
    |> should equal """
let a =
    printfn "foo: %d" 42
"""

[<Test>]
let ``nested let binding`` () =
    formatSourceString false """let a =
    let b =
        42
    b
"""  configKNA
    |> prepend newline
    |> should equal """
let a =
    let b =
        42
    b
"""

[<Test>]
let ``chained function calls`` () =
    formatSourceString false """let config =
    Builder()
      .A()
      .B()
      .C()"""  configKNA
      |> prepend newline
      |> should equal """
let config =
    Builder()
        .A()
        .B()
        .C()
"""

[<Test>]
let ``indentation incorrect for code with chained fluent interface method calls`` () =
    formatSourceString false """
let start (args: IArgs) =
    // Serilog configuration
    Log.Logger <-
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
            .Enrich.FromLogContext()
            .WriteTo.Console()
            .WriteTo.File(Path.Combine(args.ContentRoot, "temp/log.txt"))
            .CreateLogger()

    try
        try
            let giraffeApp = configureGiraffeApp args
            WebHost.CreateDefaultBuilder()
                .UseWebRoot(args.ClientPath)
                #if DEBUG
                .UseContentRoot(args.ContentRoot)
                .UseUrls(args.Host + ":" + string args.Port)
                #endif
                .UseSerilog()
                .Configure(Action<IApplicationBuilder>(configureApp giraffeApp))
                .ConfigureServices(configureServices args)
                .Build()
                .Run()
            0
        with ex ->
            Log.Fatal(ex, "Host terminated unexpectedly")
            1
    finally
        Log.CloseAndFlush()
"""  configKNA
    |> should equal """let start (args: IArgs) =
    // Serilog configuration
    Log.Logger <-
        LoggerConfiguration()
            .MinimumLevel.Debug()
            .MinimumLevel.Override("Microsoft", LogEventLevel.Information)
            .Enrich.FromLogContext()
            .WriteTo.Console()
            .WriteTo.File(Path.Combine(args.ContentRoot, "temp/log.txt"))
            .CreateLogger()

    try
        try
            let giraffeApp = configureGiraffeApp args
            WebHost.CreateDefaultBuilder().UseWebRoot(args.ClientPath)
#if DEBUG
                   .UseContentRoot(args.ContentRoot).UseUrls(args.Host + ":" + string args.Port)
#endif
                   .UseSerilog().Configure(Action<IApplicationBuilder>(configureApp giraffeApp))
                   .ConfigureServices(configureServices args).Build().Run()
            0
        with ex ->
            Log.Fatal(ex, "Host terminated unexpectedly")
            1
    finally
        Log.CloseAndFlush()
"""


[<Test>]
let ``dotGet on newline after empty string should be indented far enough`` () =
    formatSourceString false """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""  configKNA
    |> prepend newline
    |> should equal """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""

[<Test>]
let ``DotGet on newline should be indented far enough`` () =
    formatSourceString false """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""  configKNA
    |> prepend newline
    |> should equal """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""

[<Test>]
let ``Fluent api should remain on the same lines``() =
    formatSourceString false """
Log.Logger <-
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .CreateLogger()""" configKNA
    |> prepend newline
    |> should equal """
Log.Logger <-
    LoggerConfiguration()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
"""

[<Test>]
let ``newline after pattern match clauses`` () =
    formatSourceString false """
match meh with
| Foo ->
  printfn "foo"
| Bar ->
  printfn "bar"
"""  configKNA
    |> prepend newline
    |> should equal """
match meh with
| Foo ->
    printfn "foo"
| Bar ->
    printfn "bar"
"""

[<Test>]
let ``newline after match lamda`` () =
    formatSourceString false """function | Foo f ->
    printfn "bar"
    | _ -> ""
"""  configKNA
    |> should equal """function
| Foo f ->
    printfn "bar"
| _ -> ""
"""

[<Test>]
let ``elmish update`` () =
    formatSourceString false """let update msg model =
    match msg with
    | Increment when model.x < 3 ->
        { model with x = model.x + 1 }, Cmd.ofMsg Increment

    | Increment ->
        { model with x = model.x + 1 }, Cmd.ofMsg Decrement

    | Decrement when model.x > 0 ->
        { model with x = model.x - 1 }, Cmd.ofMsg Decrement

    | Decrement ->
        { model with x = model.x - 1 }, Cmd.ofMsg Increment
"""  configKNA
    |> should equal """let update msg model =
    match msg with
    | Increment when model.x < 3 ->
        { model with x = model.x + 1 }, Cmd.ofMsg Increment

    | Increment ->
        { model with x = model.x + 1 }, Cmd.ofMsg Decrement

    | Decrement when model.x > 0 ->
        { model with x = model.x - 1 }, Cmd.ofMsg Decrement

    | Decrement ->
        { model with x = model.x - 1 }, Cmd.ofMsg Increment
"""