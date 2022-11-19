module Fantomas.Core.Tests.CompilerDirectiveTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``should keep compiler directives`` () =
    formatSourceString
        false
        """
#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if INTERACTIVE
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""

[<Test>]
let ``should keep compiler directives 2`` () =
    formatSourceString
        false
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load "__setup__.fsx"
#endif
"""

[<Test>]
let ``should keep compiler directives, idempotent`` () =
    formatSourceString
        false
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""

[<Test>]
let ``line, file and path identifiers`` () =
    formatSourceString
        false
        """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """
        config
    |> prepend newline
    |> should
        equal
        """
let printSourceLocation () =
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation ()
"""

[<Test>]
let ``should keep #if, #else and #endif on compiler directives`` () =
    formatSourceString
        false
        """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = 1
#if SILVERLIGHT
let useHiddenInitCode = false
#else
let useHiddenInitCode = true
#endif
let y = 2
"""

[<Test>]
let ``should handle nested compiler directives`` () =
    formatSourceString
        false
        """
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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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

[<Test>]
let ``should handle nested compiler directives, DEBUG`` () =
    formatSourceStringWithDefines
        [ "DEBUG" ]
        """
let [<Literal>] private assemblyConfig =
    #if DEBUG
        ()
    #else
        #if TRACE
            "TRACE"
        #else
            ""
        #endif
    #endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Literal>]
let private assemblyConfig =
#if DEBUG
    ()
#else
#if TRACE
#else
#endif
#endif
"""

[<Test; Description("inactive code is not formatted correctly")>]
let ``should break lines before compiler directives`` () =
    formatSourceString
        false
        """
let [<Literal>] private assemblyConfig() =
  #if TRACE
  let x = ""
  #else
  let x = "x"
  #endif
  x
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``should break lines before compiler directives, no defines`` () =
    formatSourceStringWithDefines
        []
        """
let [<Literal>] private assemblyConfig() =
  #if TRACE
  let x = ""
  #else
  let x = "x"
  #endif
  x
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Literal>]
let private assemblyConfig () =
#if TRACE
#else
    let x = "x"
#endif
    x
"""

[<Test>]
let ``should break line after single directive`` () =
    formatSourceString
        false
        """
#nowarn "47"
namespace Internal.Utilities.Text.Lexing"""
        config
    |> prepend newline
    |> should
        equal
        """
#nowarn "47"
namespace Internal.Utilities.Text.Lexing
"""

[<Test>]
let ``should handle endif directives with no newline`` () =
    formatSourceString
        false
        """
namespace Internal.Utilities.Diagnostic

#if EXTENSIBLE_DUMPER
#if DEBUG

type ExtensibleDumper = A | B

#endif
#endif"""
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``missing inactive code if directive not defined`` () =
    formatSourceString
        false
        """
#if NOT_DEFINED
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if NOT_DEFINED
let x = 1
#endif
"""

[<Test>]
let ``don't duplicate active code if directive not defined`` () =
    formatSourceString
        false
        """
#if NOT_DEFINED
#else
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if NOT_DEFINED
#else
let x = 1
#endif
"""

[<Test>]
let ``missing line break in an active directive`` () =
    formatSourceString
        false
        """
#if DEBUG
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if DEBUG
let x = 1
#endif
"""

[<Test>]
let ``should handle #if on the first line`` () =
    formatSourceString
        false
        """
#if INTERACTIVE
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if INTERACTIVE
let x = 1
#endif
"""

[<Test>]
let ``should handle combined #if`` () =
    formatSourceString
        false
        """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""

[<Test>]
let ``should handle combined #if, INTERACTIVE`` () =
    formatSourceStringWithDefines
        [ "INTERACTIVE" ]
        """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if INTERACTIVE || (FOO && BAR) || BUZZ
let x = 1
#endif
"""

[<Test>]
let ``issue 382`` () =
    formatSourceString
        false
        """
type Currency =
    // Temporary fix until a new Thoth.Json.Net package is released
    // See https://github.com/MangelMaxime/Thoth/pull/70

#if FABLE_COMPILER
    private
#endif
    | Code of string
"""
        config
    |> should
        equal
        """type Currency =
    // Temporary fix until a new Thoth.Json.Net package is released
    // See https://github.com/MangelMaxime/Thoth/pull/70

#if FABLE_COMPILER
    private
#endif
    | Code of string
"""

[<Test>]
let ``indentation incorrect for code with chained fluent interface method calls`` () =
    formatSourceString
        false
        """
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
"""
        config
    |> should
        equal
        """let start (args: IArgs) =
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

            WebHost
                .CreateDefaultBuilder()
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
"""

[<Test>]
let ``some spacing is still lost in and around #if blocks, 303`` () =
    formatSourceString
        false
        """
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
"""
        { config with MaxInfixOperatorExpression = 75 }
    |> prepend newline
    |> should
        equal
        """
let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
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
    | Some key' ->
        assemblyName.HasPublicKey <- true
        assemblyName.PublicKey <- key'.PublicKey // sets token implicitly
#endif
"""

[<Test>]
let ``some spacing is still lost in and around #if blocks, no defines`` () =
    formatSourceStringWithDefines
        []
        """
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
"""
        { config with MaxInfixOperatorExpression = 75 }
    |> prepend newline
    |> should
        equal
        """
let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
    let assemblyName = assembly.Name
#if NETCOREAPP2_0
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
    | Some key' ->
        assemblyName.HasPublicKey <- true
        assemblyName.PublicKey <- key'.PublicKey // sets token implicitly
#endif
"""

[<Test>]
let ``some spacing is still lost in and around #if blocks, NETCOREAPP2_0`` () =
    formatSourceStringWithDefines
        [ "NETCOREAPP2_0" ]
        """
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
"""
        { config with MaxInfixOperatorExpression = 75 }
    |> prepend newline
    |> should
        equal
        """
let internal UpdateStrongNaming (assembly: AssemblyDefinition) (key: StrongNameKeyPair option) =
    let assemblyName = assembly.Name
#if NETCOREAPP2_0
    do
#else
#endif
        assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
        assemblyName.HasPublicKey <- false
        assemblyName.PublicKey <- null
        assemblyName.PublicKeyToken <- null
#if NETCOREAPP2_0
#else
#endif
"""

[<Test>]
let ``nested directives, FABLE_COMPILER`` () =
    formatSourceStringWithDefines
        [ "FABLE_COMPILER" ]
        """namespace Fable.React

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
"""
        { config with MaxDotGetExpressionWidth = 50 }
    |> should
        equal
        """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement) : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType =
            ReactBindings.React.``lazy`` (fun () ->
                // React.lazy requires a default export
                (importValueDynamic f)
                    .``then`` (fun x -> createObj [ "default" ==> x ]))

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
    formatSourceStringWithDefines
        [ "FABLE_REPL_LIB" ]
        """namespace Fable.React

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
"""
        config
    |> should
        equal
        """namespace Fable.React

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
    formatSourceStringWithDefines
        []
        """namespace Fable.React

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
"""
        config
    |> should
        equal
        """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement) : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
#else
        fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
#endif
#endif

    static member Foo = ()
"""

[<Test>]
let ``negated directive`` () =
    formatSourceString
        false
        """namespace Fable.React

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
"""
        { config with MaxDotGetExpressionWidth = 50 }
    |> should
        equal
        """namespace Fable.React

open Fable.Core
open Fable.Core.JsInterop

type FunctionComponent<'Props> = 'Props -> ReactElement
type LazyFunctionComponent<'Props> = 'Props -> ReactElement

type FunctionComponent =
#if !FABLE_REPL_LIB
    /// Creates a lazy React component from a function in another file
    /// ATTENTION: Requires fable-compiler 2.3, pass the external reference
    /// directly to the argument position (avoid pipes)
    static member inline Lazy(f: 'Props -> ReactElement, fallback: ReactElement) : LazyFunctionComponent<'Props> =
#if FABLE_COMPILER
        let elemType =
            ReactBindings.React.``lazy`` (fun () ->
                // React.lazy requires a default export
                (importValueDynamic f)
                    .``then`` (fun x -> createObj [ "default" ==> x ]))

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
    formatSourceString
        false
        """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""
        config
    |> should
        equal
        """module ReactDomBindings =
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
    formatSourceStringWithDefines
        []
        """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""
        config
    |> should
        equal
        """module ReactDomBindings =
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
    formatSourceStringWithDefines
        [ "FABLE_REPL_LIB" ]
        """module ReactDomBindings =
    #if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
    #else
    [<Import("*", "react-dom")>]
    #endif
    let ReactDom: IReactDom = jsNative

    #if !FABLE_REPL_LIB
    [<Import("default", "react-dom/server")>]
    let ReactDomServer: IReactDomServer = jsNative
    #endif"""
        config
    |> should
        equal
        """module ReactDomBindings =
#if FABLE_REPL_LIB
    [<Global("ReactDOM")>]
#else
#endif
    let ReactDom: IReactDom = jsNative

#if !FABLE_REPL_LIB
#endif
"""

[<Test>]
let ``should handle complex #if`` () =
    formatSourceString
        false
        """
#if !(INTERACTIVE || !FOO || !BAR || !BUZZ)
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if !(INTERACTIVE || !FOO || !BAR || !BUZZ)
let x = 1
#endif
"""

[<Test>]
let ``inactive code with no newline at EOF #480`` () =
    formatSourceString
        false
        """
#if NOT_DEFINED
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if NOT_DEFINED
let x = 1
#endif
"""

[<Test>]
let ``no code for inactive define`` () =
    formatSourceString
        false
        """#if SOMETHING
let foo = 42
#endif"""
        config
    |> prepend newline
    |> should
        equal
        """
#if SOMETHING
let foo = 42
#endif
"""

[<Test>]
let ``no code for inactive define, no defines`` () =
    formatSourceStringWithDefines
        []
        """#if SOMETHING
let foo = 42
#endif"""
        config
    |> prepend newline
    |> should
        equal
        """
#if SOMETHING
#endif
"""

[<Test>]
let ``#if should not be printed twice, #482`` () =
    formatSourceString
        false
        """
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
        config
    |> prepend newline
    |> should
        equal
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0
[<assembly: InternalsVisibleTo("dotnet-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
[<assembly: InternalsVisibleTo("global-altcover, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#endif
[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
[<assembly: System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""

[<Test>]
let ``482, no defines`` () =
    formatSourceStringWithDefines
        []
        """namespace AltCover

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
        config
    |> prepend newline
    |> should
        equal
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: InternalsVisibleTo("AltCover.FSApi, PublicKey=0024000004800000940000000602000000240000525341310004000001000100916443A2EE1D294E8CFA7666FB3F512D998D7CEAC4909E35EDB2AC1E104DE68890A93716D1D1931F7228AAC0523CACF50FD82CDB4CCF4FF4BF0DED95E3A383F4F371E3B82C45502CE74D7D572583495208C1905E0F1E8A3CCE66C4C75E4CA32E9A8F8DEE64E059C0DC0266E8D2CB6D7EBD464B47E062F80B63D390E389217FB7")>]
#if NETCOREAPP2_0
#endif
[<assembly: CLSCompliant(true)>]
[<assembly: ComVisible(false)>]
[<assembly: System.Resources.NeutralResourcesLanguageAttribute("en-GB")>]
let foo = ()
"""

[<Test>]
let ``hash directive between attributes, no defines`` () =
    formatSourceStringWithDefines
        []
        """[<assembly:Foo()>]
#if BAR
[<assembly:Bar()>]
#endif
[<assembly:Meh()>]
do  ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<assembly: Foo>]
#if BAR
#endif
[<assembly: Meh>]
do ()
"""

[<Test>]
let ``hash directive between attributes, bar`` () =
    formatSourceStringWithDefines
        [ "BAR" ]
        """[<assembly:Foo()>]
#if BAR
[<assembly: Bar()>]
#endif
[<assembly: Meh()>]
do  ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<assembly: Foo>]
#if BAR
[<assembly: Bar>]
#endif
[<assembly: Meh>]
do ()
"""

[<Test>]
let ``hash directive between attributes`` () =
    formatSourceString
        false
        """[<assembly:Foo()>]
#if BAR
[<assembly:Bar()>]
#endif
[<assembly:Meh()>]
do  ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<assembly: Foo>]
#if BAR
[<assembly: Bar>]
#endif
[<assembly: Meh>]
do ()
"""

[<Test>]
let ``endif in lambda`` () =
    formatSourceStringWithDefines
        [ "DEF" ]
        """foo (fun x ->
        ()
#if DEF
        ()
#endif
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
foo (fun x ->
    ()
#if DEF
    ()
#endif
)
"""

[<Test>]
let ``finally after endif`` () =
    formatSourceStringWithDefines
        [ "DEF" ]
        """try
    ()
#if DEF
    ()
#endif
finally
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        [ "DEF" ]
        """try
    ()
#if DEF
    ()
#endif
with
    | _ -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    ()
#if DEF
    ()
#endif
with _ ->
    ()
"""

[<Test>]
let ``preserve compile directive between piped functions (DEBUG), 512`` () =
    formatSourceStringWithDefines
        [ "DEBUG" ]
        """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        []
        """let foo = [ 1 ]
            |> List.sort
#if DEBUG
            |> List.rev
#endif
            |> List.sort
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo =
    [ 1 ]
    |> List.sort
#if DEBUG
#endif
    |> List.sort
"""

[<Test>]
let ``async block inside directive, 576`` () =
    formatSourceString
        false
        """#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        [ "TEST" ]
        """#if TEST
let f () =
    async {
        let x = 2
        return x
    }
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """namespace AltCover.Recorder

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
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        []
        """namespace AltCover.Recorder

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
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        [ "NET2" ]
        """namespace AltCover.Recorder

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
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        []
        """namespace global

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
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceStringWithDefines
        [ "DEBUG" ]
        """namespace global

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
"""
        { config with MaxIfThenShortWidth = 30 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """namespace global

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
"""
        { config with MaxIfThenShortWidth = 30 }
    |> prepend newline
    |> should
        equal
        """
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
    (formatSourceString
        false
        "
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
"
        config)
    |> prepend newline
    |> should
        equal
        "
[<Test>]
let ``should keep compiler directives`` () =
    formatSourceString
        false
        \"\"\"
#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"
        config
    |> should
        equal
        \"\"\"#if INTERACTIVE
#load \"../FSharpx.TypeProviders/SetupTesting.fsx\"
SetupTesting.generateSetupScript __SOURCE_DIRECTORY__
#load \"__setup__.fsx\"
#endif
\"\"\"
"

[<Test>]
let ``hash directive in single quote string should not have impact`` () =
    formatSourceString
        false
        """let a = "
#if FOO
"
let b = "
#endif
"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    "
#if FOO
"

let b =
    "
#endif
"
"""

[<Test>]
let ``hash directive in triple quote string with other quotes should not have impact`` () =
    (formatSourceString
        false
        "
let a = \"\"\"
\"
#if FOO
\"
\"\"\"
let b = \"\"\"
#endif
\"\"\"
"
        config)
    |> prepend newline
    |> should
        equal
        "
let a =
    \"\"\"
\"
#if FOO
\"
\"\"\"

let b =
    \"\"\"
#endif
\"\"\"
"

[<Test>]
let ``hash directive in single quote string should not have impact - escaped quote`` () =
    formatSourceString
        false
        """let a = "
#if FOO
\""
let b = "
#endif
"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    "
#if FOO
\""

let b =
    "
#endif
"
"""

[<Test>]
let ``defines in record assignment, no defines`` () =
    formatSourceStringWithDefines
        []
        """
let config = {
    title = "Fantomas"
    description = "Fantomas is a code formatter for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsprojects.github.io/fantomas/"
      #endif
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let config =
    { title = "Fantomas"
      description = "Fantomas is a code formatter for F#"
      theme_variant = Some "red"
      root_url =
#if WATCH
#else
        "https://fsprojects.github.io/fantomas/"
#endif
    }
"""

[<Test>]
let ``defines in record assignment, WATCH define`` () =
    formatSourceStringWithDefines
        [ "WATCH" ]
        """
let config = {
    title = "Fantomas"
    description = "Fantomas is a code formatter for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsprojects.github.io/fantomas/"
      #endif
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let config =
    { title = "Fantomas"
      description = "Fantomas is a code formatter for F#"
      theme_variant = Some "red"
      root_url =
#if WATCH
        "http://localhost:8080/"
#else
#endif
    }
"""

[<Test>]
let ``defines in record assignment, 968`` () =
    formatSourceString
        false
        """
let config = {
    title = "Fantomas"
    description = "Fantomas is a code formatter for F#"
    theme_variant = Some "red"
    root_url =
      #if WATCH
        "http://localhost:8080/"
      #else
        "https://fsprojects.github.io/fantomas/"
      #endif
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let config =
    { title = "Fantomas"
      description = "Fantomas is a code formatter for F#"
      theme_variant = Some "red"
      root_url =
#if WATCH
        "http://localhost:8080/"
#else
        "https://fsprojects.github.io/fantomas/"
#endif
    }
"""

[<Test>]
let ``compiler defines around parameter type definition, no defines`` () =
    formatSourceStringWithDefines
        []
        """
               let UpdateUI (theModel:
#if NETCOREAPP2_1
                                       ITreeModel
#else
                                       TreeModel
#endif
                             ) (info: FileInfo) () =
                 // File is good so enable the refresh button
                 h.refreshButton.Sensitive <- true
                 // Do real UI work here
                 h.classStructureTree.Model <- theModel
                 h.codeView.Buffer.Clear()
                 h.mainWindow.Title <- "AltCover.Visualizer"
                 updateMRU h info.FullName true
"""
        config
    |> prepend newline
    |> should
        equal
        """
let UpdateUI
    (theModel:
#if NETCOREAPP2_1
#else
        TreeModel
#endif
    )
    (info: FileInfo)
    ()
    =
    // File is good so enable the refresh button
    h.refreshButton.Sensitive <- true
    // Do real UI work here
    h.classStructureTree.Model <- theModel
    h.codeView.Buffer.Clear()
    h.mainWindow.Title <- "AltCover.Visualizer"
    updateMRU h info.FullName true
"""

[<Test>]
let ``compiler defines around parameter type definition, 633`` () =
    formatSourceString
        false
        """
               let UpdateUI (theModel:
#if NETCOREAPP2_1
                                       ITreeModel
#else
                                       TreeModel
#endif
                             ) (info: FileInfo) () =
                 // File is good so enable the refresh button
                 h.refreshButton.Sensitive <- true
                 // Do real UI work here
                 h.classStructureTree.Model <- theModel
                 h.codeView.Buffer.Clear()
                 h.mainWindow.Title <- "AltCover.Visualizer"
                 updateMRU h info.FullName true
"""
        config
    |> prepend newline
    |> should
        equal
        """
let UpdateUI
    (theModel:
#if NETCOREAPP2_1
        ITreeModel
#else
        TreeModel
#endif
    )
    (info: FileInfo)
    ()
    =
    // File is good so enable the refresh button
    h.refreshButton.Sensitive <- true
    // Do real UI work here
    h.classStructureTree.Model <- theModel
    h.codeView.Buffer.Clear()
    h.mainWindow.Title <- "AltCover.Visualizer"
    updateMRU h info.FullName true
"""

[<Test>]
let ``directives under parsed hash directives`` () =
    formatSourceString
        false
        """
#load "../../.paket/load/netstandard2.0/Client/client.group.fsx"
#load "../Overview/Types.fs"
#load "./Shared.fsx"
#load "../Overview/Types.fs"

#if INTERACTIVE
#r "netstandard"
#endif

open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Teams.Overview.Types
open Browser.Types
open Browser
open Shared
"""
        config
    |> prepend newline
    |> should
        equal
        """
#load "../../.paket/load/netstandard2.0/Client/client.group.fsx"
#load "../Overview/Types.fs"
#load "./Shared.fsx"
#load "../Overview/Types.fs"

#if INTERACTIVE
#r "netstandard"
#endif

open Fable.React
open Fable.React.Props
open Fable.Core
open Fable.Core.JsInterop
open Teams.Overview.Types
open Browser.Types
open Browser
open Shared
"""

[<Test>]
let ``namespace under parsed hash directives, 2014`` () =
    formatSourceString
        false
        """
#load "Types.fsx"
#load "Project.fsx"

namespace MyNamespace
"""
        config
    |> prepend newline
    |> should
        equal
        """
#load "Types.fsx"
#load "Project.fsx"

namespace MyNamespace
"""

[<Test>]
let ``empty module with trivia, FAKE`` () =
    formatSourceStringWithDefines
        [ "FAKE" ]
        """
// This file is automatically generated by FAKE
// This file is needed for IDE support only
#if !FAKE
#load "intellisense_lazy.fsx"
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
// This file is automatically generated by FAKE
// This file is needed for IDE support only
#if !FAKE
#endif
"""

[<Test>]
let ``empty module with trivia, 1031`` () =
    formatSourceString
        false
        """
// This file is automatically generated by FAKE
// This file is needed for IDE support only
#if !FAKE
#load "intellisense_lazy.fsx"
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
// This file is automatically generated by FAKE
// This file is needed for IDE support only
#if !FAKE
#load "intellisense_lazy.fsx"
#endif
"""

[<Test>]
let ``don't indent too far after multiple hash directives, 1026`` () =
    formatSourceString
        false
        """
let getDefaultProxyFor =
    memoize
      (fun (url:string) ->
            let uri = Uri url
            let getDefault () =
#if CUSTOM_WEBPROXY
                let result =
                    { new IWebProxy with
                        member __.Credentials
                            with get () = null
                            and set _value = ()
                        member __.GetProxy _ = null
                        member __.IsBypassed (_host : Uri) = true
                    }
#else
                let result = WebRequest.GetSystemWebProxy()
#endif
#if CUSTOM_WEBPROXY
                let proxy = result
#else
                let address = result.GetProxy uri
                if address = uri then null else
                let proxy = WebProxy address
                proxy.BypassProxyOnLocal <- true
#endif
                proxy.Credentials <- CredentialCache.DefaultCredentials
                proxy

            match calcEnvProxies.Force().TryFind uri.Scheme with
            | Some p -> if p.GetProxy uri <> uri then p else getDefault()
            | None -> getDefault())
"""
        { config with MaxIfThenElseShortWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
let getDefaultProxyFor =
    memoize (fun (url: string) ->
        let uri = Uri url

        let getDefault () =
#if CUSTOM_WEBPROXY
            let result =
                { new IWebProxy with
                    member __.Credentials
                        with get () = null
                        and set _value = ()

                    member __.GetProxy _ = null
                    member __.IsBypassed(_host: Uri) = true }
#else
            let result = WebRequest.GetSystemWebProxy()
#endif
#if CUSTOM_WEBPROXY
            let proxy = result
#else
            let address = result.GetProxy uri

            if address = uri then
                null
            else
                let proxy = WebProxy address
                proxy.BypassProxyOnLocal <- true
#endif
            proxy.Credentials <- CredentialCache.DefaultCredentials
            proxy

        match calcEnvProxies.Force().TryFind uri.Scheme with
        | Some p -> if p.GetProxy uri <> uri then p else getDefault ()
        | None -> getDefault ())
"""

[<Test>]
let ``backslashes in strings prior to hash directives should not affect token parsing of those directives, 1205`` () =
    formatSourceString
        false
        """
let loadFile n =
  let file =
    System.IO.Path.Combine(contentDir,
                           (n |> System.IO.Path.GetFileNameWithoutExtension)
                           + ".md").Replace("\\", "/")

  ()

let loader (projectRoot: string) (siteContent: SiteContents) =
#if WATCH
  let disableLiveRefresh = false
#else
  let disableLiveRefresh = true
#endif
  disableLiveRefresh
"""
        { config with
            MaxDotGetExpressionWidth = 50
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let loadFile n =
    let file =
        System
            .IO
            .Path
            .Combine(
                contentDir,
                (n |> System.IO.Path.GetFileNameWithoutExtension)
                + ".md"
            )
            .Replace("\\", "/")

    ()

let loader (projectRoot: string) (siteContent: SiteContents) =
#if WATCH
    let disableLiveRefresh = false
#else
    let disableLiveRefresh = true
#endif
    disableLiveRefresh
"""

[<Test>]
let ``directive above SynExpr.Do, 1333`` () =
    formatSourceString
        false
        """
[<AutoOpen>]
module ReactHookExtensions =
    type React with
        [<Hook>]
        static member useDeferred(operation: Async<'T>, dependencies: obj array) =
            let (deferred, setDeferred) = React.useState(Deferred.HasNotStartedYet)
            let token = React.useCancellationToken()
            let executeOperation = async {
                try
                    do setDeferred(Deferred<'T>.InProgress)
                    let! output = operation
                    do setDeferred(Deferred<'T>.Resolved output)
                with error ->
                    #if DEBUG
                    Browser.Dom.console.log(error)
                    #endif
                    do setDeferred(Deferred<'T>.Failed error)
            }

            React.useEffect((fun () -> Async.StartImmediate(executeOperation, token.current)), dependencies)

            deferred
"""
        { config with NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
[<AutoOpen>]
module ReactHookExtensions =
    type React with
        [<Hook>]
        static member useDeferred(operation: Async<'T>, dependencies: obj array) =
            let (deferred, setDeferred) = React.useState (Deferred.HasNotStartedYet)
            let token = React.useCancellationToken ()

            let executeOperation =
                async {
                    try
                        do setDeferred (Deferred<'T>.InProgress)
                        let! output = operation
                        do setDeferred (Deferred<'T>.Resolved output)
                    with error ->
#if DEBUG
                        Browser.Dom.console.log (error)
#endif
                        do setDeferred (Deferred<'T>.Failed error)
                }

            React.useEffect ((fun () -> Async.StartImmediate(executeOperation, token.current)), dependencies)

            deferred
"""

[<Test>]
let ``simple hash directive consider as one trivia`` () =
    formatSourceStringWithDefines
        []
        """
let x =
    #if DEBUG
    printfn "DEBUG"
    #endif
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
#if DEBUG
#endif
    ()
"""

[<Test>]
let ``hash if and hash else should be one trivia`` () =
    formatSourceStringWithDefines
        []
        """
#if FOO
                printfn "FOO"
#else
                ()
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#else
()
#endif
"""

[<Test>]
[<Ignore "not support for now">]
let ``hash nested in multiline string`` () =
    formatSourceStringWithDefines
        []
        "
#if FOO
    \"\"\"
    #if BAR
                printfn \"FOO\"
    #endif
    \"\"\"
#else
                ()
#endif
"
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#else
()
#endif
"""

[<Test>]
[<Ignore "not support for now">]
let ``hash nested in multiline block comment`` () =
    formatSourceStringWithDefines
        []
        """
#if FOO
    (*
        #if BAR
                    printfn "FOO"
        #endif
    *)
#else
                ()
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
    (*
        #if BAR
                    printfn "FOO"
        #endif
    *)
#else
()
#endif
"""

[<Test>]
let ``empty hash directive block should not make expression multiline`` () =
    formatSourceString
        false
        """
    do
#if FOOBAR

#endif
        assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
        assemblyName.HasPublicKey <- false
        assemblyName.PublicKey <- null
        assemblyName.PublicKeyToken <- null
"""
        { config with MaxInfixOperatorExpression = 75 }
    |> prepend newline
    |> should
        equal
        """
do
#if FOOBAR

#endif
    assembly.MainModule.Attributes <- assembly.MainModule.Attributes &&& (~~~ModuleAttributes.StrongNameSigned)
    assemblyName.HasPublicKey <- false
    assemblyName.PublicKey <- null
    assemblyName.PublicKeyToken <- null
"""

[<Test>]
let ``comment after compiler define`` () =
    formatSourceString
        false
        """
#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
    let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy = FreshenTyconRef m (if isExtrinsic then TyparRigidity.Flexible else rigid) tcref declaredTyconTypars
#else
    let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy = FreshenTyconRef m rigid tcref declaredTyconTypars
#endif
"""
        { config with MaxIfThenElseShortWidth = 20 }
    |> prepend newline
    |> should
        equal
        """
#if EXTENDED_EXTENSION_MEMBERS // indicates if extension members can add additional constraints to type parameters
let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy =
    FreshenTyconRef
        m
        (if isExtrinsic then
             TyparRigidity.Flexible
         else
             rigid)
        tcref
        declaredTyconTypars
#else
let tcrefObjTy, enclosingDeclaredTypars, renaming, objTy =
    FreshenTyconRef m rigid tcref declaredTyconTypars
#endif
"""

[<Test>]
let ``content after #else and #endif, 2293`` () =
    formatSourceString
        false
        """
#if FOO
#else // xxx
#endif // yyy
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#else // xxx
#endif // yyy
"""

[<Test>]
let ``defines as trivia for SynExpr.TypeApp, 1543`` () =
    formatSourceString
        false
        """
let inputFileFlagsFsiBase (_tcConfigB: TcConfigBuilder) =
#if NETSTANDARD
    [ CompilerOption("usesdkrefs", tagNone, OptionSwitch (SetUseSdkSwitch _tcConfigB), None, Some (FSComp.SR.useSdkRefs())) ]
#else
    List.empty<CompilerOption>
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inputFileFlagsFsiBase (_tcConfigB: TcConfigBuilder) =
#if NETSTANDARD
    [ CompilerOption(
          "usesdkrefs",
          tagNone,
          OptionSwitch(SetUseSdkSwitch _tcConfigB),
          None,
          Some(FSComp.SR.useSdkRefs ())
      ) ]
#else
    List.empty<CompilerOption>
#endif
"""

[<Test>]
let ``define before opening bracket of array, 1597`` () =
    formatSourceString
        false
        """
    let Environment = { new IEnvironment with
        member _.IsWindows() =
            InteropServices.RuntimeInformation.IsOSPlatform(InteropServices.OSPlatform.Windows)

        member _.GetScriptArgs() =
    #if INTERACTIVE
            fsi.CommandLineArgs
            |> Array.skip 1
    #else
            [||]
    #endif

        member _.GetEnvironmentVariable(varName) =
            System.Environment.GetEnvironmentVariable(varName)

        member _.SetEnvironmentVariable(varName, value) =
            System.Environment.SetEnvironmentVariable(varName, value)
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Environment =
    { new IEnvironment with
        member _.IsWindows() =
            InteropServices.RuntimeInformation.IsOSPlatform(InteropServices.OSPlatform.Windows)

        member _.GetScriptArgs() =
#if INTERACTIVE
            fsi.CommandLineArgs |> Array.skip 1
#else
            [||]
#endif

        member _.GetEnvironmentVariable(varName) =
            System.Environment.GetEnvironmentVariable(varName)

        member _.SetEnvironmentVariable(varName, value) =
            System.Environment.SetEnvironmentVariable(varName, value) }
"""

[<Test>]
let ``hash directive above recursive let binding inside type definition, 1776`` () =
    formatSourceString
        false
        """
    type ObjectGraphFormatter(opts: FormatOptions, bindingFlags) =
        let rec nestedObjL depthLim prec (x:obj, ty:Type) =
            objL ShowAll depthLim prec (x, ty)
        and stringValueL (s: string) =
            countNodes 1
#if COMPILER
            ()
#else
            wordL (tagStringLiteral (formatString s))
#endif

        and arrayValueL depthLim (arr: Array) =
            ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ObjectGraphFormatter(opts: FormatOptions, bindingFlags) =
    let rec nestedObjL depthLim prec (x: obj, ty: Type) = objL ShowAll depthLim prec (x, ty)

    and stringValueL (s: string) =
        countNodes 1
#if COMPILER
        ()
#else
        wordL (tagStringLiteral (formatString s))
#endif

    and arrayValueL depthLim (arr: Array) = ()
"""

[<Test>]
let ``verbatim string is ignore for hash directive scan,  1794`` () =
    formatSourceString
        false
        """
let ProgramFilesX86 =
    match wow64, globalArch with
    | "AMD64", "AMD64"
    | null, "AMD64"
    | "x86", "AMD64" -> Environment.GetEnvironmentVariable "ProgramFiles(x86)"
    | _ -> Environment.GetEnvironmentVariable "ProgramFiles"
    |> fun detected -> if detected = null then @"C:\Program Files (x86)\" else detected

let isUnix =
#if NETSTANDARD1_6 || NETSTANDARD2_0
    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
        System.Runtime.InteropServices.OSPlatform.Linux) ||
    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
        System.Runtime.InteropServices.OSPlatform.OSX)
#else
    int Environment.OSVersion.Platform |> fun p -> (p = 4) || (p = 6) || (p = 128)
#endif
"""
        { config with MaxInfixOperatorExpression = 40 }
    |> prepend newline
    |> should
        equal
        """
let ProgramFilesX86 =
    match wow64, globalArch with
    | "AMD64", "AMD64"
    | null, "AMD64"
    | "x86", "AMD64" -> Environment.GetEnvironmentVariable "ProgramFiles(x86)"
    | _ -> Environment.GetEnvironmentVariable "ProgramFiles"
    |> fun detected ->
        if detected = null then
            @"C:\Program Files (x86)\"
        else
            detected

let isUnix =
#if NETSTANDARD1_6 || NETSTANDARD2_0
    System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Linux)
    || System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.OSX)
#else
    int Environment.OSVersion.Platform
    |> fun p -> (p = 4) || (p = 6) || (p = 128)
#endif
"""

[<Test>]
let ``indented #if directive inside another non-indented #if directive should format correctly, 1866`` () =
    formatSourceString
        false
        """
#if FOO
    #if BAR
    #endif
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#if BAR
#endif
#endif
"""

[<Test>]
let ``double try-with, inner #if directive should not throw error, 1969`` () =
    formatSourceString
        false
        """
try
    try
        ()
#if FOO
        ()
#endif
    with
    | _ -> ()
with
| _ -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    try
        ()
#if FOO
        ()
#endif
    with _ ->
        ()
with _ ->
    ()
"""

[<Test>]
let ``should handle #if with boolean constant`` () =
    formatSourceString
        false
        """
#if false
let x = 1
#endif
#if true
let x = 1
#endif
"""
        config
    |> should
        equal
        """#if false
let x = 1
#endif
#if true
let x = 1
#endif
"""

[<Test>]
let ``type definition in signature file wrapped with hash directives, 1115`` () =
    formatSourceString
        true
        """
namespace X

type UnresolvedAssemblyReference = UnresolvedAssemblyReference of string * AssemblyReference list

#if !NO_EXTENSIONTYPING
type ResolvedExtensionReference = ResolvedExtensionReference of string * AssemblyReference list * Tainted<ITypeProvider> list
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace X

type UnresolvedAssemblyReference = UnresolvedAssemblyReference of string * AssemblyReference list

#if !NO_EXTENSIONTYPING
type ResolvedExtensionReference =
    | ResolvedExtensionReference of string * AssemblyReference list * Tainted<ITypeProvider> list
#endif
"""

[<Test>]
let ``using a compiler directive should not copy the previous line in fsi files, 1186`` () =
    formatSourceString
        true
        """
module Foo

type t
val x : int

#if DEBUG
val y : int
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type t
val x: int

#if DEBUG
val y: int
#endif
"""

[<Test>]
let ``content of #if block should not get removed, 801`` () =
    formatSourceString
        false
        """
#if !COMPILED

#I "C:/devuser/AppData/Roaming/npm/node_modules/azure-functions-core-tools/bin"
#r "Microsoft.Azure.WebJobs.Host.dll"
#r "System.Net.Http.Formatting.dll"

open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Host

#endif

#load "../shared/utilities.fsx"

open Vspan.Common.Utilities

#load "DialoutFunction.fsx"

open Vspan.Domain.Functions

let Run(message: string, executionContext: ExecutionContext, log: TraceWriter) =
    let logInfo = log.Info
    let getSetting = System.Environment.GetEnvironmentVariable

    executionContext
    |> FunctionGuid logInfo
    |> ignore

    message |> Out.Dialout.DialoutFunction.Accept logInfo getSetting
"""
        { config with MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
#if !COMPILED

#I "C:/devuser/AppData/Roaming/npm/node_modules/azure-functions-core-tools/bin"
#r "Microsoft.Azure.WebJobs.Host.dll"
#r "System.Net.Http.Formatting.dll"

open Microsoft.Azure.WebJobs
open Microsoft.Azure.WebJobs.Host

#endif

#load "../shared/utilities.fsx"

open Vspan.Common.Utilities

#load "DialoutFunction.fsx"

open Vspan.Domain.Functions

let Run (message: string, executionContext: ExecutionContext, log: TraceWriter) =
    let logInfo = log.Info
    let getSetting = System.Environment.GetEnvironmentVariable

    executionContext |> FunctionGuid logInfo |> ignore

    message
    |> Out.Dialout.DialoutFunction.Accept logInfo getSetting
"""

[<Test>]
let ``nested defines, all active code`` () =
    formatSourceStringWithDefines
        [ "FOO"; "BAR" ]
        """
#if FOO
    #if BAR
        ()
    #endif
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#if BAR
()
#endif
#endif
"""

[<Test>]
let ``nested defines, all dead code`` () =
    formatSourceStringWithDefines
        [ "BAR" ]
        """
#if FOO
    #if BAR
        ()
    #endif
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#if BAR
#endif
#endif
"""

[<Test>]
let ``dead code in active block`` () =
    formatSourceStringWithDefines
        [ "FOO" ]
        """
#if FOO
    #if BAR
        a
    #endif
    b
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
#if FOO
#if BAR
#endif
b
#endif
"""
