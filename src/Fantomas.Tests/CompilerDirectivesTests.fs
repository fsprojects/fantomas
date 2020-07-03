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
let printSourceLocation () =
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation ()
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
let private assemblyConfig () =
#if TRACE
    let x = ""
#else
    let x = "x"
#endif
    x
"""

[<Test>]
let ``should break lines before compiler directives, no defines``() =
    formatSourceStringWithDefines [] """
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
let private assemblyConfig () =
#if TRACE

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
    | Code of string
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
        LoggerConfiguration().MinimumLevel.Debug().MinimumLevel.Override("Microsoft", LogEventLevel.Information)
            .Enrich.FromLogContext().WriteTo.Console().WriteTo.File(Path.Combine(args.ContentRoot, "temp/log.txt"))
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
"""  ({ config with MaxInfixOperatorExpression = 75 })
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

[<Test>]
let ``nested directives, FABLE_COMPILER`` () =
    formatSourceStringWithDefines ["FABLE_COMPILER"] """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement,
                                fallback: ReactElement)
                            : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType = ReactBindings.React.``lazy``(fun () ->
            // React.lazy requires a default export
            (importValueDynamic f).``then``(fun x -> createObj ["default" ==> x]))
        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj ["fallback" ==> fallback])
                [ReactElementType.create elemType props []]
#else
        fun _ ->
            div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""  config
    |> should equal """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement): LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType =
            ReactBindings.React.``lazy`` (fun () ->
                // React.lazy requires a default export
                (importValueDynamic f).``then``(fun x -> createObj [ "default" ==> x ]))

        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj [ "fallback" ==> fallback ])
                [ ReactElementType.create elemType props [] ]
#else


#endif
#endif

    static member Foo = ()
"""

[<Test>]
let ``nested directives, FABLE_REPL_LIB`` () =
    formatSourceStringWithDefines ["FABLE_REPL_LIB"] """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement,
                                fallback: ReactElement)
                            : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType = ReactBindings.React.``lazy``(fun () ->
            // React.lazy requires a default export
            (importValueDynamic f).``then``(fun x -> createObj ["default" ==> x]))
        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj ["fallback" ==> fallback])
                [ReactElementType.create elemType props []]
#else
        fun _ ->
            div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""  config
    |> should equal """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB






#if FABLE_COMPILER








#else


#endif
#endif

    static member Foo = ()
"""

[<Test>]
let ``nested directives, no defines`` () =
    formatSourceStringWithDefines [] """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement,
                                fallback: ReactElement)
                            : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType = ReactBindings.React.``lazy``(fun () ->
            // React.lazy requires a default export
            (importValueDynamic f).``then``(fun x -> createObj ["default" ==> x]))
        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj ["fallback" ==> fallback])
                [ReactElementType.create elemType props []]
#else
        fun _ ->
            div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""  config
    |> should equal """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement): LazyFunctionComponent<'Props> =
#if FABLE_COMPILER








#else
        fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""

[<Test>]
let ``negated directive`` () =
    formatSourceString false """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement,
                                fallback: ReactElement)
                            : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType = ReactBindings.React.``lazy``(fun () ->
            // React.lazy requires a default export
            (importValueDynamic f).``then``(fun x -> createObj ["default" ==> x]))
        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj ["fallback" ==> fallback])
                [ReactElementType.create elemType props []]
#else
        fun _ ->
            div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""  config
    |> should equal """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement): LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType =
            ReactBindings.React.``lazy`` (fun () ->
                // React.lazy requires a default export
                (importValueDynamic f).``then``(fun x -> createObj [ "default" ==> x ]))

        fun props ->
            ReactElementType.create
                ReactBindings.React.Suspense
                (createObj [ "fallback" ==> fallback ])
                [ ReactElementType.create elemType props [] ]
#else
        fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""

[<Test>]
let ``module with nested directives`` () =
    formatSourceString false """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""  config
    |> should equal """module ReactDomBindings =
#if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
#else
    [<Import("*", "react-dom")>]
#endif
    let ReactDom: IReactDom = jsNative

#if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
#endif
"""

[<Test>]
let ``module with nested directives, no defines`` () =
    formatSourceStringWithDefines [] """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""  config
    |> should equal """module ReactDomBindings =
#if FABLE_REPL_LIB

#else
    [<Import("*", "react-dom")>]
#endif
    let ReactDom: IReactDom = jsNative

#if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
#endif
"""

[<Test>]
let ``module with nested directives, FABLE_REPL_LIB`` () =
    formatSourceStringWithDefines ["FABLE_REPL_LIB"] """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""  config
    |> should equal """module ReactDomBindings =
#if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
#else

#endif
    let ReactDom: IReactDom = jsNative

#if !FABLE_REPL_LIB


#endif
"""

[<Test>]
let ``should handle complex #if``() =
    formatSourceString false """
#if !(INTERACTIVE || !FOO || !BAR || !BUZZ)
let x = 1
#endif
"""  config
    |> should equal """#if !(INTERACTIVE || !FOO || !BAR || !BUZZ)
let x = 1
#endif
"""

[<Test>]
let ``inactive code with no newline at EOF #480``() =
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
let ``no code for inactive define`` () =
    formatSourceString false """#if SOMETHING
let foo = 42
#endif"""  config
    |> prepend newline
    |> should equal """
#if SOMETHING
let foo = 42
#endif
"""

[<Test>]
let ``#if should not be printed twice, #482`` () =
    formatSourceString false """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly:InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0
[<assembly:InternalsVisibleTo("dotnet-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
[<assembly:InternalsVisibleTo("global-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#endif
[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
[<assembly:System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""  config
    |> prepend newline
    |> should equal """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly:InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0
[<assembly:InternalsVisibleTo("dotnet-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
[<assembly:InternalsVisibleTo("global-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#endif
[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
[<assembly:System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""

[<Test>]
let ``482, no defines`` () =
    formatSourceStringWithDefines [] """namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly:InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0
[<assembly:InternalsVisibleTo("dotnet-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
[<assembly:InternalsVisibleTo("global-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#endif
[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
[<assembly:System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""  config
    |> prepend newline
    |> should equal """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly:InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0


#endif
[<assembly:CLSCompliant(true)>]
[<assembly:ComVisible(false)>]
[<assembly:System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""

[<Test>]
let ``hash directive between attributes`` () =
    formatSourceStringWithDefines [] """[<assembly:Foo()>]
#if BAR
[<assembly:Bar()>]
#endif
[<assembly:Meh()>]
do  ()
"""  config
    |> prepend newline
    |> should equal """
[<assembly:Foo>]
#if BAR

#endif
[<assembly:Meh>]

do ()
"""

[<Test>]
let ``hash directive between attributes, bar`` () =
    formatSourceStringWithDefines ["BAR"] """[<assembly:Foo()>]
#if BAR
[<assembly:Bar()>]
#endif
[<assembly:Meh()>]
do  ()
"""  config
    |> prepend newline
    |> should equal """
[<assembly:Foo>]
#if BAR
[<assembly:Bar>]
#endif
[<assembly:Meh>]

do ()
"""

[<Test>]
let ``endif in lambda`` () =
    formatSourceStringWithDefines ["DEF"] """foo (fun x ->
        ()
#if DEF
        ()
#endif
)
"""  config
    |> prepend newline
    |> should equal """
foo (fun x ->
        ()
#if DEF
        ()
#endif
    )
"""

[<Test>]
let ``finally after endif`` () =
    formatSourceStringWithDefines ["DEF"] """try
    ()
#if DEF
    ()
#endif
finally
    ()
"""  config
    |> prepend newline
    |> should equal """
try
    ()
#if DEF
    ()
#endif
finally
    ()
"""

[<Test>]
let ``with after endif`` () =
    formatSourceStringWithDefines ["DEF"] """try
    ()
#if DEF
    ()
#endif
with
    | _ -> ()
"""  config
    |> prepend newline
    |> should equal """
try
    ()
#if DEF
    ()
#endif
with _ -> ()
"""

[<Test>]
let ``preserve compile directive between piped functions (DEBUG), 512`` () =
    formatSourceStringWithDefines ["DEBUG"] """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""  config
    |> prepend newline
    |> should equal """
let foo =
    [ 1 ]
    |> List.sort
#if DEBUG
    |> List.rev
#endif
    |> List.sort
"""

[<Test>]
let ``preserve compile directive between piped functions, 512`` () =
    formatSourceString false """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""  config
    |> prepend newline
    |> should equal """
let foo =
    [ 1 ]
    |> List.sort
#if DEBUG
    |> List.rev
#endif
    |> List.sort
"""

[<Test>]
let ``preserve compile directive between piped functions, DEBUG`` () =
    formatSourceStringWithDefines [] """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""  config
    |> prepend newline
    |> should equal """
let foo =
    [ 1 ]
    |> List.sort
#if DEBUG

#endif
    |> List.sort
"""

[<Test>]
let ``async block inside directive, 576`` () =
    formatSourceString false """#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""  config
    |> prepend newline
    |> should equal """
#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""

[<Test>]
let ``async block inside directive, TEST`` () =
    formatSourceStringWithDefines ["TEST"] """#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""  config
    |> prepend newline
    |> should equal """
#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""

[<Test>]
let ``directive capturing attribute, 635`` () =
    formatSourceString false """namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""  ({ config with IndentSize = 2 })
    |> prepend newline
    |> should equal """
namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""

[<Test>]
let ``directive capturing attribute, no defines`` () =
    formatSourceStringWithDefines [] """namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""  ({ config with IndentSize = 2 })
    |> prepend newline
    |> should equal """
namespace AltCover.Recorder

open System

#if NET2

#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""

[<Test>]
let ``directive capturing attribute, NET2`` () =
    formatSourceStringWithDefines ["NET2"] """namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""  ({ config with IndentSize = 2 })
    |> prepend newline
    |> should equal """
namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else

#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
"""


[<Test>]
let ``namespace global mixed with hash directives, no directives`` () =
    formatSourceStringWithDefines [] """namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn = if condition() then fn()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
"""  config
    |> prepend newline
    |> should equal """
namespace global

#if DEBUG
















#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
"""

[<Test>]
let ``namespace global mixed with hash directives, DEBUG`` () =
    formatSourceStringWithDefines ["DEBUG"] """namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn = if condition() then fn()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
"""  config
    |> prepend newline
    |> should equal """
namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn = if condition () then fn ()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else




#endif
"""

[<Test>]
let ``namespace global mixed with hash directives, 681`` () =
    formatSourceString false """namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn = if condition() then fn()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
"""  config
    |> prepend newline
    |> should equal """
namespace global

#if DEBUG

module Dbg =

    open System
    open System.Text

    let seq fn = Seq.iter fn

    let iff condition fn = if condition () then fn ()

    let tee fn a =
        fn a
        a

    let teePrint x = tee (printfn "%A") x
    let print x = printfn "%A" x
#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
"""

[<Test>]
let ``defines in string should be taken into account, 761`` () =
    (formatSourceString false "
[<Test>]
let ``should keep compiler directives``() =
    formatSourceString false \"\"\"
#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"  config
    |> should equal \"\"\"#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"
" config)
    |> prepend newline
    |> should equal "
[<Test>]
let ``should keep compiler directives`` () =
    formatSourceString false \"\"\"
#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"  config
    |> should equal \"\"\"#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"
"

[<Test>]
let ``hash directive in single quote string should not have impact`` () =
    formatSourceString false """let a = "
#if FOO
"
let b = "
#endif
"
"""  config
    |> prepend newline
    |> should equal """
let a = "
#if FOO
"

let b = "
#endif
"
"""

[<Test>]
let ``hash directive in triple quote string with other quotes should not have impact`` () =
    (formatSourceString false "
let a = \"\"\"
\"
#if FOO
\"
\"\"\"
let b = \"\"\"
#endif
\"\"\"
"     config)
    |> prepend newline
    |> should equal "
let a = \"\"\"
\"
#if FOO
\"
\"\"\"

let b = \"\"\"
#endif
\"\"\"
"

[<Test>]
let ``hash directive in single quote string should not have impact - escaped quote`` () =
    formatSourceString false """let a = "
#if FOO
\""
let b = "
#endif
"
"""  config
    |> prepend newline
    |> should equal """
let a = "
#if FOO
\""

let b = "
#endif
"
"""
