module Fantomas.Tests.CompilerDirectiveTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``should use verbatim strings on some hash directives``() =
    formatSourceString false """
    #r @"C:\foo\bar.dll"
    """ config
    |> prepend newline
    |> should equal """
#r @"C:\foo\bar.dll"
"""

[<Test>]
let ``hash directives``() =
    formatSourceString false """
    #r "Fantomas.Tests.dll"
    #load "CodeFormatterTests.fs"
    """ config
    |> prepend newline
    |> should equal """
#r "Fantomas.Tests.dll"
#load "CodeFormatterTests.fs"
"""

[<Test>]
let ``should support load directive multiple arguments``() =
    formatSourceString false """
    #load "A.fs" "B.fs"
    #load "C.fs"
          "D.fs"
          "E.fs"
    """ config
    |> prepend newline
    |> should equal """
#load "A.fs" "B.fs"
#load "C.fs" "D.fs" "E.fs"
"""

[<Test>]
let ``should keep compiler directives``() =
    formatSourceString false """
#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""  config
    |> should equal """#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""

[<Test>]
let ``should keep compiler directives 2``() =
    formatSourceString false """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""  config
    |> should equal """#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""

[<Test>]
let ``line, file and path identifiers``() =
    formatSourceString false """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """ config
    |> prepend newline
    |> should equal """
let printSourceLocation() =
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation()
"""

[<Test>]
let ``should keep #if, #else and #endif on compiler directives``() =
    formatSourceString false """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""  config
    |> prepend newline
    |> should equal """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""

[<Test>]
let ``should handle nested compiler directives``() =
    formatSourceString false """
let [<Literal>] private assemblyConfig =
    #if DEBUG
    #if TRACE
    "DEBUG;TRACE"
    #else
    "DEBUG"
    #endif
    #else
    #if TRACE
    "TRACE"
    #else
    ""
    #endif
    #endif
"""  config
    |> prepend newline
    |> should equal """
[<Literal>]
let private assemblyConfig =
    #if DEBUG
    #if TRACE
    "DEBUG;TRACE"
    #else
    "DEBUG"
    #endif
    #else
    #if TRACE
    "TRACE"
    #else
    ""
    #endif
    #endif
"""

[<Test; Description("inactive code is not formatted correctly")>]
let ``should break lines before compiler directives``() =
    formatSourceString false """
let [<Literal>] private assemblyConfig() =
  #if TRACE
  let x = ""
  #else
  let x = "x"
  #endif
  x
"""  config
    |> prepend newline
    |> should equal """
[<Literal>]
let private assemblyConfig() =
    #if TRACE
    let x = ""
    #else
    let x = "x"
    #endif
    x
"""

[<Test>]
let ``should break line after single directive``() =
    formatSourceString false """
#nowarn "47"
namespace Internal.Utilities.Text.Lexing"""  config
    |> prepend newline
    |> should equal """
#nowarn "47"
namespace Internal.Utilities.Text.Lexing

"""

[<Test>]
let ``should handle endif directives with no newline``() =
    formatSourceString false """
namespace Internal.Utilities.Diagnostic

#if EXTENSIBLE_DUMPER
#if DEBUG

type ExtensibleDumper = A | B

#endif  
#endif"""  config
    |> prepend newline
    |> should equal """
namespace Internal.Utilities.Diagnostic

#if EXTENSIBLE_DUMPER
#if DEBUG

type ExtensibleDumper =
    | A
    | B

#endif
#endif
"""

[<Test>]
let ``missing inactive code if directive not defined``() =
    formatSourceString false """
#if NOT_DEFINED
let x = 1
#endif
"""  config
    |> should equal """#if NOT_DEFINED
let x = 1
#endif
"""

[<Test>]
let ``don't duplicate active code if directive not defined``() =
    formatSourceString false """
#if NOT_DEFINED
#else
let x = 1
#endif
"""  config
    |> should equal """#if NOT_DEFINED
#else
let x = 1
#endif
"""

[<Test>]
let ``missing line break in an active directive``() =
    formatSourceString false """
#if DEBUG
let x = 1
#endif
"""  config
    |> should equal """#if DEBUG
let x = 1
#endif
"""

[<Test>]
let ``should handle #if on the first line``() =
    formatSourceString false """
#if INTERACTIVE
let x = 1
#endif
"""  config
    |> should equal """#if INTERACTIVE
let x = 1
#endif
"""

[<Test>]
let ``should handle combined #if``() =
    formatSourceString false """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""  config
    |> should equal """#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""

[<Test>]
let ``issue 382`` () =
    formatSourceString false """
type Currency =
    // Temporary fix until a new Thoth.Json.Net package is released
    // See https://github.com/MangelMaxime/Thoth/pull/70

#if FABLE_COMPILER
    private
#endif
    | Code of string
"""  config
    |> should equal """type Currency =
    // Temporary fix until a new Thoth.Json.Net package is released
    // See https://github.com/MangelMaxime/Thoth/pull/70

    #if FABLE_COMPILER
    private
    #endif
    Code of string
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
"""  config
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
let ``some spacing is still lost in and around #if blocks, 303`` () =
    formatSourceString false """
  let internal UpdateStrongNaming (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    let assemblyName = assembly.Name
#if NETCOREAPP2_0
    do
#else
    match key with
    | None ->
#endif
              assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
              assemblyName.HasPublicKey <- false
              assemblyName.PublicKey <- null
              assemblyName.PublicKeyToken <- null
#if NETCOREAPP2_0
#else
    | Some key' -> assemblyName.HasPublicKey <- true
                   assemblyName.PublicKey <- key'.PublicKey // sets token implicitly
#endif
"""  config
    |> should equal """let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
    let assemblyName = assembly.Name
    #if NETCOREAPP2_0
    #else
    match key with
    | None ->
    #endif
    do assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
       assemblyName.HasPublicKey <- false
       assemblyName.PublicKey <- null
       assemblyName.PublicKeyToken <- null
    #if NETCOREAPP2_0
    #else
    | Some key' ->
        assemblyName.HasPublicKey <- true
        assemblyName.PublicKey <- key'.PublicKey // sets token implicitly
       #endif
"""