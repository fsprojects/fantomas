module Fantomas.Core.Tests.DotGetTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

[<Test>]
let ``a TypeApp inside a DotGet should stay on the same line, 994`` () =
    formatSourceString
        """
Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<option<option<unit>>>.GetGenericTypeDefinition().MakeGenericType(t)).Assembly
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
Microsoft.FSharp.Reflection.FSharpType
    .GetUnionCases(
        typeof<option<option<unit>>>
            .GetGenericTypeDefinition()
            .MakeGenericType(t)
    )
    .Assembly
"""

[<Test>]
let ``a DotGetApp inside a DotGet should stay on the same line, 1051`` () =
    formatSourceString
        """
System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion
"""
        { config with
            MaxLineLength = 80
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
System.Diagnostics.FileVersionInfo
    .GetVersionInfo(
        System.Reflection.Assembly
            .GetExecutingAssembly()
            .Location
    )
    .FileVersion
"""

[<Test>]
let ``split chained method call expression, 246`` () =
    formatSourceString
        """
        root.SetAttribute
          ("driverVersion",
           "AltCover.Recorder "
           + System.Diagnostics.FileVersionInfo.GetVersionInfo(
               System.Reflection.Assembly.GetExecutingAssembly().Location).FileVersion)
"""
        { config with
            MaxDotGetExpressionWidth = 50
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
root.SetAttribute(
    "driverVersion",
    "AltCover.Recorder "
    + System.Diagnostics.FileVersionInfo
        .GetVersionInfo(
            System.Reflection.Assembly
                .GetExecutingAssembly()
                .Location
        )
        .FileVersion
)
"""

[<Test>]
let ``keep parenthesis on same line as SynExpr.TypeApp`` () =
    formatSourceString
        """
Equinox.EventStore.Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy).Resolve
"""
        { config with MaxLineLength = 100 }
    |> prepend newline
    |> should
        equal
        """
Equinox.EventStore
    .Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy)
    .Resolve
"""

[<Test>]
let ``don't break line for generic function call, 1134`` () =
    formatSourceString
        """
module Services =
    /// Builds a Stream Resolve function appropriate to the store being used
    type StreamResolver(storage: Storage.Instance) =
        member __.Resolve
            (
                codec: FsCodec.IEventCodec<'event, byte[], _>,
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Services =
    /// Builds a Stream Resolve function appropriate to the store being used
    type StreamResolver(storage: Storage.Instance) =
        member __.Resolve
            (
                codec: FsCodec.IEventCodec<'event, byte[], _>,
                fold: ('state -> 'event seq -> 'state),
                initial: 'state,
                snapshot: (('event -> bool) * ('state -> 'event))
            ) =
            match storage with
            | Storage.MemoryStore store ->
                Equinox.MemoryStore
                    .Resolver(store, FsCodec.Box.Codec.Create(), fold, initial)
                    .Resolve
            | Storage.EventStore(gateway, cache) ->
                let accessStrategy = Equinox.EventStore.AccessStrategy.RollingSnapshots snapshot

                let cacheStrategy =
                    Equinox.EventStore.CachingStrategy.SlidingWindow(cache, TimeSpan.FromMinutes 20.)

                Equinox.EventStore
                    .Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy)
                    .Resolve
"""

[<Test>]
let ``long chained expression should be multiline, 501`` () =
    formatSourceString
        """
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``nested TypeApp inside DotGet`` () =
    formatSourceString
        """
let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .WithIdentity(taskName, groupName)
        .Build()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let job =
    JobBuilder
        .UsingJobData(jobDataMap)
        .Create<WrapperJob>()
        .WithIdentity(taskName, groupName)
        .Build()
"""

[<Test>]
let ``TypeApp at end of nested DotGet`` () =
    formatSourceString
        """
let c =
      builder
        .CaptureStartupErrors(true)
        .UseSerilog(dispose = true)
        .UseStartup<Startup>()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c =
    builder
        .CaptureStartupErrors(true)
        .UseSerilog(dispose = true)
        .UseStartup<Startup>()
"""

[<Test>]
let ``inner SynExpr.LongIdent should also be split`` () =
    formatSourceString
        """
let firstName =
    define
        .Attribute
        .ParsedRes(FirstName.value, FirstName.create)
        .Get(fun u -> u.FirstName)
        .SetRes(userSetter User.setFirstName)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let firstName =
    define.Attribute
        .ParsedRes(FirstName.value, FirstName.create)
        .Get(fun u -> u.FirstName)
        .SetRes(userSetter User.setFirstName)
"""

[<Test>]
let ``long ident with dots inside app inside dotget`` () =
    formatSourceString
        """
Equinox.MemoryStore.Resolver(store, FsCodec.Box.Codec.Create(), fold, initial)
                    .Resolve
"""
        config
    |> prepend newline
    |> should
        equal
        """
Equinox.MemoryStore
    .Resolver(store, FsCodec.Box.Codec.Create(), fold, initial)
    .Resolve
"""

[<Test>]
let ``long ident with dots inside type app inside dotget`` () =
    formatSourceString
        """
                Equinox.EventStore.Resolver<'event, 'state, _>(gateway,
                                                               codec,
                                                               fold,
                                                               initial,
                                                               cacheStrategy,
                                                               accessStrategy)
                    .Resolve
"""
        config
    |> prepend newline
    |> should
        equal
        """
Equinox.EventStore
    .Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy)
    .Resolve
"""

[<Test>]
let ``lambda should have extra indent inside dotget`` () =
    formatSourceString
        """
let getColl =
  define
    .Operation
    .ForContext(Context.toAuthenticatedContext)
    .GetCollection(fun _ parser ->
      let x = 2
      x)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let getColl =
    define.Operation
        .ForContext(Context.toAuthenticatedContext)
        .GetCollection(fun _ parser ->
            let x = 2
            x)
"""

[<Test>]
let ``dotget app lambda`` () =
    formatSourceString
        """
let getColl =
    GetCollection(fun _ parser ->
        let x = 1
        x
    ).ToString()

let getColl2 =
    GetCollection(fun parser ->
        let x = 2
        x
    ).ToString()

let getColl3 =
    GetCollection(fun _ parser ->
        let x = 3
        x
    ).Foo

let getColl4 =
    GetCollection(fun parser ->
        let x = 4
        x
    ).Foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
let getColl =
    GetCollection(fun _ parser ->
        let x = 1
        x)
        .ToString()

let getColl2 =
    GetCollection(fun parser ->
        let x = 2
        x)
        .ToString()

let getColl3 =
    GetCollection(fun _ parser ->
        let x = 3
        x)
        .Foo

let getColl4 =
    GetCollection(fun parser ->
        let x = 4
        x)
        .Foo
"""

[<Test>]
let ``comment between chained call`` () =
    formatSourceString
        """
Log
    .Foo()
    // Bar
    .Poo()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Log
    .Foo()
    // Bar
    .Poo()
"""

[<Test>]
let ``short DotGetApp with unit`` () =
    formatSourceString
        """
Foo().Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().Bar()
"""

[<Test>]
let ``short DotGetApp with lowercase function name and unit`` () =
    formatSourceString
        """
Foo().bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().bar ()
"""

[<Test>]
let ``short DotGetApp with constant`` () =
    formatSourceString
        """
Foo().Bar "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().Bar "meh"
"""

[<Test>]
let ``short DotGetApp with property`` () =
    formatSourceString
        """
Foo().Bar().Length
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().Bar().Length
"""

[<Test>]
let ``short DotGetApp with multiline idents and constant`` () =
    formatSourceString
        """
MyModule.Foo().Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
MyModule.Foo().Bar()
"""

[<Test>]
let ``short DotGet TypedApp`` () =
    formatSourceString
        """
typeof<System.Collections.IEnumerable>.FullName
"""
        config
    |> prepend newline
    |> should
        equal
        """
typeof<System.Collections.IEnumerable>.FullName
"""

[<Test>]
let ``short DotGet with lambda`` () =
    formatSourceString
        """
Foo(fun x -> x).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo(fun x -> x).Bar()
"""

[<Test>]
let ``named argument inside DotGet application`` () =
    formatSourceString
        """
SomeFunction(name = SearchForName(
    "foooooooooooooooooooooooooooooooooooooooooooooooooo",
    "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
)).ChainedFunctionCall()
"""
        config
    |> prepend newline
    |> should
        equal
        """
SomeFunction(
    name =
        SearchForName(
            "foooooooooooooooooooooooooooooooooooooooooooooooooo",
            "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
        )
)
    .ChainedFunctionCall()
"""

[<Test>]
let ``named argument inside DotGet application, SpaceBeforeUppercaseInvocation`` () =
    formatSourceString
        """
SomeFunction(name = SearchForName(
    "foooooooooooooooooooooooooooooooooooooooooooooooooo",
    "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
)).ChainedFunctionCall()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
SomeFunction(
    name =
        SearchForName (
            "foooooooooooooooooooooooooooooooooooooooooooooooooo",
            "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
        )
)
    .ChainedFunctionCall ()
"""

[<Test>]
let ``space before uppercase invocation should only be respected at end of chain, 1438`` () =
    formatSourceString
        """
namespace AspNetSerilog

[<Extension>]
type IWebHostBuilderExtensions() =

    [<Extension>]
    static member UseSerilog(webHostBuilder : IWebHostBuilder, index : Index) =
        webHostBuilder.UseSerilog(fun context configuration ->
            configuration
                .MinimumLevel.Debug()
                .WriteTo.Logger(fun loggerConfiguration ->
                    loggerConfiguration
                        .Enrich.WithProperty("host", Environment.MachineName)
                        .Enrich.WithProperty("user", Environment.UserName)
                        .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
                    |> ignore
                )
            |> ignore
        )
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
namespace AspNetSerilog

[<Extension>]
type IWebHostBuilderExtensions() =

    [<Extension>]
    static member UseSerilog(webHostBuilder: IWebHostBuilder, index: Index) =
        webHostBuilder.UseSerilog (fun context configuration ->
            configuration.MinimumLevel
                .Debug()
                .WriteTo.Logger (fun loggerConfiguration ->
                    loggerConfiguration.Enrich
                        .WithProperty("host", Environment.MachineName)
                        .Enrich.WithProperty("user", Environment.UserName)
                        .Enrich.WithProperty (
                            "application",
                            context.HostingEnvironment.ApplicationName
                        )
                    |> ignore
                )
            |> ignore
        )
"""

[<Test>]
let ``space before uppercase invocation only on last lid of chain, 1437`` () =
    formatSourceString
        """
Log.Logger <-
    LoggerConfiguration()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
Log.Logger <-
    LoggerConfiguration()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger ()
"""

[<Test>]
let ``space before uppercase invocation only on last lid of chain, tupled arg`` () =
    formatSourceString
        """
Log.Logger <-
    LoggerConfiguration(1,2)
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
"""
        { config with
            SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
Log.Logger <-
    LoggerConfiguration(1, 2)
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger ()
"""

[<Test>]
let ``don't add space when function call is followed by lambda, 1440`` () =
    formatSourceString
        """
let blah =
    Mock().Returns(fun _ ->
                {
                    dasdasdsadsadsadsa = ""
                    Sdadsadasdasdas =  "sdsadsadasdsa"
                })
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
let blah =
    Mock()
        .Returns (fun _ ->
            {
                dasdasdsadsadsadsa = ""
                Sdadsadasdasdas = "sdsadsadasdsa"
            }
        )
"""

[<Test>]
let ``don't add space when function call is followed by lambda, const expr`` () =
    formatSourceString
        """
let blah =
    Mock("foo").Returns(fun _ ->
                {
                    dasdasdsadsadsadsa = ""
                    Sdadsadasdasdas =  "sdsadsadasdsa"
                })
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
let blah =
    Mock("foo")
        .Returns (fun _ ->
            {
                dasdasdsadsadsadsa = ""
                Sdadsadasdasdas = "sdsadsadasdsa"
            }
        )
"""

[<Test>]
let ``typedApp should not have space with chained DotGet, 1447`` () =
    formatSourceString
        """
let x =
                        LoggerConfiguration<Foo>("host", Environment.MachineName)
                            .Enrich.WithProperty<Bar>("user", Environment.UserName)
                            .Enrich.WithProperty ("application", context.HostingEnvironment.ApplicationName)
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeMember = true }
    |> prepend newline
    |> should
        equal
        """
let x =
    LoggerConfiguration<Foo>("host", Environment.MachineName)
        .Enrich.WithProperty<Bar>("user", Environment.UserName)
        .Enrich.WithProperty ("application", context.HostingEnvironment.ApplicationName)
"""

[<Test>]
let ``typedApp should not have space with chained DotGet, unit arg`` () =
    formatSourceString
        """
let x =
                        LoggerConfiguration<Foo>()
                            .Enrich.WithProperty<Bar>("user", Environment.UserName)
                            .Enrich.WithProperty ("application", context.HostingEnvironment.ApplicationName)
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeMember = true }
    |> prepend newline
    |> should
        equal
        """
let x =
    LoggerConfiguration<Foo>()
        .Enrich.WithProperty<Bar>("user", Environment.UserName)
        .Enrich.WithProperty ("application", context.HostingEnvironment.ApplicationName)
"""

[<Test>]
let ``typeApp followed by chained lambda, 1448`` () =
    formatSourceString
        """
let blah =
    Mock<Foo>()
        .Returns (fun _ ->
            { dasdasdsadsadsadsa = ""
              Sdadsadasdasdas = "sdsadsadasdsa" })
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeMember = true }
    |> prepend newline
    |> should
        equal
        """
let blah =
    Mock<Foo>()
        .Returns (fun _ ->
            { dasdasdsadsadsadsa = ""
              Sdadsadasdasdas = "sdsadsadasdsa" })
"""

[<Test>]
let ``avoid name-sensitive alignments, 1422`` () =
    formatSourceString
        """
let retrySql<'a> =
  Policy
    .HandleTransientSqlError()
    .WaitAndRetryAsync(
      List.map TimeSpan.FromSeconds [ 1.; 2.; 3. ],
      fun ex ts i ctx ->
        Log.Information(
          ex,
          "DB retry policy: Exception thrown, performing retry {RetryNo}, operation {OperationKey}",
          i,
          ctx.OperationKey
        ))
    .AsAsyncPolicy<'a>()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let retrySql<'a> =
    Policy
        .HandleTransientSqlError()
        .WaitAndRetryAsync(
            List.map TimeSpan.FromSeconds [ 1.; 2.; 3. ],
            fun ex ts i ctx ->
                Log.Information(
                    ex,
                    "DB retry policy: Exception thrown, performing retry {RetryNo}, operation {OperationKey}",
                    i,
                    ctx.OperationKey
                )
        )
        .AsAsyncPolicy<'a>()
"""

[<Test>]
let ``dotget in multiline infix expression, 1521`` () =
    formatSourceString
        """
let PublishValueDefn cenv env declKind (vspec: Val) =
    if (declKind = ModuleOrMemberBinding) &&
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) &&
       (Option.isNone vspec.MemberInfo) then
           errorR(Error(FSComp.SR.tcNamespaceCannotContainValues(), vspec.Range))

    if (declKind = ExtrinsicExtensionBinding) &&
       ((GetCurrAccumulatedModuleOrNamespaceType env).ModuleOrNamespaceKind = Namespace) then
           errorR(Error(FSComp.SR.tcNamespaceCannotContainExtensionMembers(), vspec.Range))

    ()
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
let PublishValueDefn cenv env declKind (vspec: Val) =
    if
        (declKind = ModuleOrMemberBinding)
        && ((GetCurrAccumulatedModuleOrNamespaceType env)
               .ModuleOrNamespaceKind = Namespace)
        && (Option.isNone vspec.MemberInfo)
    then
        errorR (Error(FSComp.SR.tcNamespaceCannotContainValues (), vspec.Range))

    if
        (declKind = ExtrinsicExtensionBinding)
        && ((GetCurrAccumulatedModuleOrNamespaceType env)
               .ModuleOrNamespaceKind = Namespace)
    then
        errorR (Error(FSComp.SR.tcNamespaceCannotContainExtensionMembers (), vspec.Range))

    ()
"""

[<Test>]
let ``DotGetApp inside multiline infix expression should indent, 1529`` () =
    formatSourceString
        """
type Foobar =
    member tcConfig.IsSystemAssembly (filename: string) =
        try
            FileSystem.SafeExists filename &&
            ((tcConfig.GetTargetFrameworkDirectories() |> List.exists (fun clrRoot -> clrRoot = Path.GetDirectoryName filename)) ||
             (tcConfig.FxResolver.GetSystemAssemblies().Contains (fileNameWithoutExtension filename)) ||
             tcConfig.FxResolver.IsInReferenceAssemblyPackDirectory filename)
        with _ ->
            false
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foobar =
    member tcConfig.IsSystemAssembly(filename: string) =
        try
            FileSystem.SafeExists filename
            && ((tcConfig.GetTargetFrameworkDirectories()
                 |> List.exists (fun clrRoot -> clrRoot = Path.GetDirectoryName filename))
                || (tcConfig.FxResolver
                       .GetSystemAssemblies()
                       .Contains(fileNameWithoutExtension filename))
                || tcConfig.FxResolver.IsInReferenceAssemblyPackDirectory filename)
        with _ ->
            false
"""

[<Test>]
let ``lambda dotget with TypeApp in chain, 1550`` () =
    formatSourceString
        """
services
    .AddIdentityCore<web.ApplicationUser>(fun options ->
            options.User.RequireUniqueEmail <- true
            options.SignIn.RequireConfirmedEmail <- true)
    .AddUserManager<UserManager<web.ApplicationUser>>()
"""
        config
    |> prepend newline
    |> should
        equal
        """
services
    .AddIdentityCore<web.ApplicationUser>(fun options ->
        options.User.RequireUniqueEmail <- true
        options.SignIn.RequireConfirmedEmail <- true)
    .AddUserManager<UserManager<web.ApplicationUser>>()
"""

[<Test>]
let ``lambda dotget with TypeApp in chain in single line`` () =
    formatSourceString
        """
services
    .AddIdentityCore(fun options -> ())
    .AddUserManager<UserManager<web.ApplicationUser>>()
"""
        { config with
            MaxDotGetExpressionWidth = 200 }
    |> prepend newline
    |> should
        equal
        """
services.AddIdentityCore(fun options -> ()).AddUserManager<UserManager<web.ApplicationUser>>()
"""

[<Test>]
let ``parenthesis argument expression inside DotGetApp chain, 1651`` () =
    formatSourceString
        """
module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo.FromDirectoryName(fs.Path.Combine((ThingThing.rootRoot fs thingThing).FullName, "tada!")).EnumerateDirectories()
            |> Seq.exactlyOne
        ()
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo
                .FromDirectoryName(
                    fs.Path.Combine ((ThingThing.rootRoot fs thingThing).FullName, "tada!")
                )
                .EnumerateDirectories ()
            |> Seq.exactlyOne

        ()
"""

[<Test>]
let ``parenthesis argument expression inside DotGetApp chain, max line 80`` () =
    formatSourceString
        """
module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo.FromDirectoryName(fs.Path.Combine((ThingThing.rootRoot fs thingThing).FullName, "tada!")).EnumerateDirectories()
            |> Seq.exactlyOne
        ()
"""
        { config with
            MaxLineLength = 80
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar () =
        let saveDir =
            fs.DirectoryInfo
                .FromDirectoryName(
                    fs.Path.Combine (
                        (ThingThing.rootRoot fs thingThing).FullName,
                        "tada!"
                    )
                )
                .EnumerateDirectories ()
            |> Seq.exactlyOne

        ()
"""

[<Test>]
let ``single line dotget lambda, followed by application`` () =
    formatSourceString
        """
Foo(fun x ->  x).Bar().Meh
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo(fun x -> x).Bar().Meh
"""

[<Test>]
let ``multiline dotget lambda, followed by application, 1662`` () =
    formatSourceString
        """
type Class() =
    member this.``kk``() =
        async {
            mock
                .Setup(fun m ->
                m.CreateBlah
                    (It.IsAny<string>(), It.IsAny<string>(), It.IsAny<Id>(), It.IsAny<uint32>()))
                .Returns(Some mock)
                .End
        }
        |> Async.StartImmediate
"""
        { config with MaxLineLength = 119 }
    |> prepend newline
    |> should
        equal
        """
type Class() =
    member this.``kk``() =
        async {
            mock
                .Setup(fun m ->
                    m.CreateBlah(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<Id>(), It.IsAny<uint32>()))
                .Returns(Some mock)
                .End
        }
        |> Async.StartImmediate
"""

[<Test>]
let ``multiline dotget lambda, followed by multiple applications`` () =
    formatSourceString
        """
mock
                .Setup(fun m ->
                // some comment
                m.CreateBlah
                    (It.IsAny<string>(), It.IsAny<string>(), It.IsAny<Id>(), It.IsAny<uint32>()))
                .Returns(Some mock)
                .OrNot()
                .End
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
mock
    .Setup(fun m ->
        // some comment
        m.CreateBlah(
            It.IsAny<string>(),
            It.IsAny<string>(),
            It.IsAny<Id>(),
            It.IsAny<uint32>()
        ))
    .Returns(Some mock)
    .OrNot()
    .End
"""

[<Test>]
let ``dotget function application should add space before argument, short`` () =
    formatSourceString
        """
m.Property(fun p -> p.Name).HasMaxLength 64
"""
        config
    |> prepend newline
    |> should
        equal
        """
m.Property(fun p -> p.Name).HasMaxLength 64
"""

[<Test>]
let ``dotget function application should add space before argument, long`` () =
    formatSourceString
        """
m.Property(fun p -> p.Name).IsRequired().HasColumnName("ModelName").HasMaxLength 64
"""
        { config with
            MaxDotGetExpressionWidth = 70 }
    |> prepend newline
    |> should
        equal
        """
m
    .Property(fun p -> p.Name)
    .IsRequired()
    .HasColumnName("ModelName")
    .HasMaxLength
    64
"""

[<Test>]
let ``dotget lambda multiline application`` () =
    formatSourceString
        """
m.Property(fun p -> p.Name).IsRequired().HasColumnName("ModelName").HasMaxLength
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
m
    .Property(fun p -> p.Name)
    .IsRequired()
    .HasColumnName("ModelName")
    .HasMaxLength
"""

[<Test>]
let ``dotget chain with a lambda and ending in multiline function application, 1804`` () =
    formatSourceString
        """
db.Schema.Users.Query
    .Where(fun x -> x.Role)
    .Matches(function Role.User companyId -> companyId |_->__)
    .In(
        db.Schema.Companies.Query
            .Where(fun x -> x.LicenceId).Equals(licenceId)
            .Select(fun x -> x.Id)
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
db.Schema.Users.Query
    .Where(fun x -> x.Role)
    .Matches(
        function
        | Role.User companyId -> companyId
        | _ -> __
    )
    .In(
        db.Schema.Companies.Query
            .Where(fun x -> x.LicenceId)
            .Equals(licenceId)
            .Select(fun x -> x.Id)
    )
"""

[<Test>]
let ``trivia between LongIdentWithDots in DotGet, 2098`` () =
    formatSourceString
        """
namespace PmaBolero.Client

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection.Extensions
open Bolero.Remoting.Client

module Program =

    [<EntryPoint>]
    let Main args =
        let builder = WebAssemblyHostBuilder.CreateDefault(args)
        builder.RootComponents.Add<Pages.Main.MyApp>("#main")
        builder.Services
            .AddRemoting(builder.HostEnvironment)
            .Services
#if (!DEBUG)
            .RemoveAll<Microsoft.Extensions.Http.IHttpMessageHandlerBuilderFilter>()
#endif
            |> ignore
        builder.Build().RunAsync() |> ignore
        0
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
namespace PmaBolero.Client

open Microsoft.AspNetCore.Components.WebAssembly.Hosting
open Microsoft.Extensions.DependencyInjection.Extensions
open Bolero.Remoting.Client

module Program =

    [<EntryPoint>]
    let Main args =
        let builder = WebAssemblyHostBuilder.CreateDefault(args)
        builder.RootComponents.Add<Pages.Main.MyApp>("#main")

        builder.Services
            .AddRemoting(builder.HostEnvironment)
            .Services
#if (!DEBUG)
            .RemoveAll<Microsoft.Extensions.Http.IHttpMessageHandlerBuilderFilter>()
#endif
        |> ignore

        builder.Build().RunAsync() |> ignore
        0
"""

[<Test>]
let ``dotget inside a quotation, 2154`` () =
    formatSourceString
        """
(fun (Singleton arg) -> <@@ ((%%arg: Indicators) :> IIndicators).AsyncGetIndicator(indicatorIdVal) @@>)
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
(fun (Singleton arg) ->
    <@@
        ((%%arg: Indicators) :> IIndicators)
            .AsyncGetIndicator(indicatorIdVal)
    @@>)
"""

[<Test>]
let ``argument in short type app as function name, 2683`` () =
    formatSourceString
        """
Assembly.GetExecutingAssembly().GetCustomAttribute<MyCustomAttribute>().SomeProperty
"""
        config
    |> prepend newline
    |> should
        equal
        """
Assembly
    .GetExecutingAssembly()
    .GetCustomAttribute<MyCustomAttribute>()
    .SomeProperty
"""
