module Fantomas.Core.Tests.SynLongIdentTests

open Fantomas.FCS.Text
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``fluent api should not remain on the same lines`` () =
    formatSourceString
        """
Log.Logger <-
  LoggerConfiguration()
    .Destructure.FSharpTypes()
    .WriteTo.Console()
    .CreateLogger()"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
Log.Logger <-
    LoggerConfiguration()
        .Destructure.FSharpTypes()
        .WriteTo.Console()
        .CreateLogger()
"""

[<Test>]
let ``fluent api with comments should remain on same lines`` () =
    formatSourceString
        """
Log.Logger <-
  LoggerConfiguration()
   // Suave.SerilogExtensions has native destructuring mechanism
   // this helps Serilog deserialize the fsharp types like unions/records
   .Destructure.FSharpTypes()
   // use package Serilog.Sinks.Console
   // https://github.com/serilog/serilog-sinks-console
   .WriteTo.Console()
   // add more sinks etc.
   .CreateLogger()"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        """let config = //
    Builder()//
        .UseCaching()//
        .UseSql()//
        .UseMeh()
"""
        config
    |> should
        equal
        """let config = //
    Builder() //
        .UseCaching() //
        .UseSql() //
        .UseMeh()
"""

[<Test>]
let ``method call on multiple lines`` () =
    formatSourceString
        """module Program

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
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Program

[<EntryPoint>]
let main _ =
    try
        try
            Config.Logger.configure ()

            let config =
                ConfigurationBuilder().SetBasePath(Directory.GetCurrentDirectory()).Build()

            WebHostBuilder()
                .UseConfiguration(config)
                .UseKestrel()
                .UseSerilog()
                .ConfigureAppConfiguration(
                    Action<WebHostBuilderContext, IConfigurationBuilder> configureAppConfiguration
                )
                .ConfigureServices(Action<WebHostBuilderContext, IServiceCollection> configureServices)
                .Configure(Action<IApplicationBuilder> configureApp)
                .Build()
                .Run()
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
    formatSourceString
        """namespace LoginWithBulmaTutorial

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
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
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

        MySPA()
            .AttrPassword(Attr.ClassPred "is-danger" (not passwordValid.V))
            .AttrEmail(Attr.ClassPred "is-danger" (not emailValid.V))
            .Login(fun e ->
                passwordValid
                := not (String.IsNullOrWhiteSpace e.Vars.Password.Value)

                emailValid
                := not (String.IsNullOrWhiteSpace e.Vars.Email.Value)

                if passwordValid.Value && emailValid.Value then
                    JS.Alert(sprintf "Your email is %s" e.Vars.Email.Value)

                e.Event.PreventDefault())
            .Bind()
"""

[<Test>]
let ``don't repeat parenthesis for DotGet Paren, 989`` () =
    formatSourceString
        """(something_really_long
  + another_thing_thats_really_long).A
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
(something_really_long
 + another_thing_thats_really_long)
    .A
"""

[<Test>]
let ``infix expression inside DotGet, 921`` () =
    formatSourceString
        """let variable =
                (DataAccess.getById moduleName.readData
                         { Id = createObject.Id }
                     |> Result.okValue).Value
"""
        config
    |> prepend newline
    |> should
        equal
        """
let variable =
    (DataAccess.getById moduleName.readData { Id = createObject.Id }
     |> Result.okValue)
        .Value
"""

[<Test>]
let ``preserve comment before SynExpr.LongIdent, 1080`` () =
    formatSourceString
        """
let shrinkInput input =
    match toSynExprs input with
    | [] ->
        //stdout.WriteLine("Can't shrink {0} further.", sprintf "%A" input)
        Seq.empty
"""
        config
    |> prepend newline
    |> should
        equal
        """
let shrinkInput input =
    match toSynExprs input with
    | [] ->
        //stdout.WriteLine("Can't shrink {0} further.", sprintf "%A" input)
        Seq.empty
"""

[<Test>]
let ``comment in LongIdent application, 2062`` () =
    formatSourceString
        """
Rollbar
  .RollbarLocator
  .RollbarInstance
  // .AsBlockingLogger(System.TimeSpan.FromSeconds 5)
  .Error(package, custom)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Rollbar
    .RollbarLocator
    .RollbarInstance
    // .AsBlockingLogger(System.TimeSpan.FromSeconds 5)
    .Error(package, custom)
"""

[<Test>]
let ``comment inside LongIdentWithDots preserved, 2027`` () =
    formatSourceString
        """
let path =
    match normalizedPath with
    | path ->
        path  // translate path to Python relative syntax
            .Replace("../../../", "....")
"""
        config
    |> prepend newline
    |> should
        equal
        """
let path =
    match normalizedPath with
    | path ->
        path // translate path to Python relative syntax
            .Replace("../../../", "....")
"""

[<Test>]
let ``backticks can be added from AST only scenarios`` () =
    let tree =
        let testIdent = Ident("Test", Range.Zero)

        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "Test.fsx",
                true,
                QualifiedNameOfFile testIdent,
                [],
                [],
                [ SynModuleOrNamespace(
                      [ testIdent ],
                      false,
                      SynModuleOrNamespaceKind.AnonModule,
                      [ SynModuleDecl.Expr(
                            SynExpr.LongIdent(
                                false,
                                SynLongIdent(
                                    [ Ident("new", Range.Zero) ],
                                    [],
                                    [ Some(IdentTrivia.OriginalNotation "``new``") ]
                                ),
                                None,
                                Range.Zero
                            ),
                            Range.Zero
                        ) ],
                      PreXmlDoc.Empty,
                      [],
                      None,
                      Range.Zero,
                      { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
                  ) ],
                (true, false),
                { ConditionalDirectives = []
                  CodeComments = [] },
                Set.empty
            )
        )

    CodeFormatter.FormatASTAsync(
        tree,
        config =
            { config with
                InsertFinalNewline = false }
    )
    |> Async.RunSynchronously
    |> should equal "``new``"
