module Fantomas.Tests.DotGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``a TypeApp inside a DotGet should stay on the same line, 994`` () =
    formatSourceString false """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t)).Assembly
"""  config
    |> prepend newline
    |> should equal """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>
        .GetGenericTypeDefinition()
        .MakeGenericType(t))
    .Assembly
"""

[<Test>]
let ``a DotGetApp inside a DotGet should stay on the same line, 1051`` () =
    formatSourceString false """
System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion
"""  { config with MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly()
        .Location)
    .FileVersion
"""

[<Test>]
let ``split chained method call expression, 246`` () =
    formatSourceString false """
        root.SetAttribute
          ("driverVersion",
           "AltCover.Recorder "
           + System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion)
"""  config
    |> prepend newline
    |> should equal """
root.SetAttribute
    ("driverVersion",
     "AltCover.Recorder "
     + System.Diagnostics.FileVersionInfo.GetVersionInfo(System.Reflection.Assembly.GetExecutingAssembly()
             .Location)
         .FileVersion)
"""

[<Test>]
let ``keep parenthesis on same line as SynExpr.TypeApp`` () =
    formatSourceString false """
Equinox.EventStore.Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy).Resolve
"""  { config with MaxLineLength = 100 }
    |> prepend newline
    |> should equal """
Equinox.EventStore.Resolver<'event, 'state, _>(gateway,
                                               codec,
                                               fold,
                                               initial,
                                               cacheStrategy,
                                               accessStrategy)
    .Resolve
"""

[<Test>]
let ``don't break line for generic function call, 1134`` () =
    formatSourceString false """
module Services =
    /// Builds a Stream Resolve function appropriate to the store being used
    type StreamResolver(storage: Storage.Instance) =
        member __.Resolve
            (
                codec: FsCodec.IEventCodec<'event, byte [], _>,
                fold: ('state -> 'event seq -> 'state),
                initial: 'state,
                snapshot: (('event -> bool) * ('state -> 'event))
            )
            =
            match storage with
            | Storage.MemoryStore store ->
                Equinox.MemoryStore.Resolver(store, FsCodec.Box.Codec.Create(), fold, initial).Resolve
            | Storage.EventStore (gateway, cache) ->
                let accessStrategy =
                    Equinox.EventStore.AccessStrategy.RollingSnapshots snapshot

                let cacheStrategy =
                    Equinox.EventStore.CachingStrategy.SlidingWindow(cache, TimeSpan.FromMinutes 20.)

                Equinox.EventStore.Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy).Resolve
"""  config
    |> prepend newline
    |> should equal """
module Services =
    /// Builds a Stream Resolve function appropriate to the store being used
    type StreamResolver(storage: Storage.Instance) =
        member __.Resolve(codec: FsCodec.IEventCodec<'event, byte [], _>,
                          fold: ('state -> 'event seq -> 'state),
                          initial: 'state,
                          snapshot: (('event -> bool) * ('state -> 'event))) =
            match storage with
            | Storage.MemoryStore store ->
                Equinox.MemoryStore.Resolver(store, FsCodec.Box.Codec.Create(), fold, initial)
                    .Resolve
            | Storage.EventStore (gateway, cache) ->
                let accessStrategy =
                    Equinox.EventStore.AccessStrategy.RollingSnapshots snapshot

                let cacheStrategy =
                    Equinox.EventStore.CachingStrategy.SlidingWindow(cache, TimeSpan.FromMinutes 20.)

                Equinox.EventStore.Resolver<'event, 'state, _>(gateway,
                                                               codec,
                                                               fold,
                                                               initial,
                                                               cacheStrategy,
                                                               accessStrategy)
                    .Resolve
"""

[<Test>]
let ``long chained expression should be multiline, 501`` () =
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
