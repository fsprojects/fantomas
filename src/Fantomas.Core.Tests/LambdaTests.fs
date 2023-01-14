module Fantomas.Core.Tests.LambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core.FormatConfig

[<Test>]
let ``keep comment after arrow`` () =
    formatSourceString
        false
        """_Target "FSharpTypesDotNet" (fun _ -> // obsolete
 ())
"""
        { config with
            IndentSize = 2
            MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
_Target "FSharpTypesDotNet" (fun _ -> // obsolete
  ())
"""

let ``indent multiline lambda in parenthesis, 523`` () =
    formatSourceString
        false
        """let square = (fun b ->
    b*b
    prinftn "%i" b*b
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let square =
    (fun b ->
        b * b
        prinftn "%i" b * b)
"""

[<Test>]
let ``lambda inside tupled argument`` () =
    formatSourceString
        false
        """#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>
        ((fun _ ->
            fragment []
                [ h3 []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ]]), "BadgeSample")

exportDefault badgeSample
"""
        { config with MaxArrayOrListWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
#load "../../.paket/load/netstandard2.0/main.group.fsx"
#load "../../src/Common.fs"
#load "../../src/Badge.fs"

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Reactstrap

let private badgeSample =
    FunctionComponent.Of<obj>(
        (fun _ ->
            fragment
                []
                [ h3
                      []
                      [ str "Heading "
                        Badge.badge [ Badge.Color Secondary ] [ str "New" ] ]
                  Badge.badge [ Badge.Color Warning ] [ str "oh my" ] ]),
        "BadgeSample"
    )

exportDefault badgeSample
"""

[<Test>]
let ``long identifier inside lambda`` () =
    formatSourceString
        false
        """
let a =
    b
    |> List.exists (fun p ->
        x && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let a =
    b
    |> List.exists (fun p ->
        x
        && someVeryLongIdentifierrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrzzzz___________)
"""

[<Test>]
let ``FAKE target`` () =
    formatSourceString
        false
        """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Target.create "Clean" (fun _ ->
    [ "bin"
      "src/Fantomas/bin"
      "src/Fantomas/obj"
      "src/Fantomas.CoreGlobalTool/bin"
      "src/Fantomas.CoreGlobalTool/obj" ]
    |> List.iter Shell.cleanDir)
"""

[<Test>]
let ``destructed argument lamba`` () =
    formatSourceString
        false
        """
List.filter (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.filter (fun ({ ContentBefore = contentBefore }) ->
    // some comment
    let a = 8
    let b = List.length contentBefore
    a + b)
"""

[<Test>]
let ``destructed argument lamba in let binding`` () =
    formatSourceString
        false
        """
let a =
    (fun ({ ContentBefore = contentBefore }) ->
                                                    // some comment
                                                    let a = 8
                                                    let b = List.length contentBefore
                                                    a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    (fun ({ ContentBefore = contentBefore }) ->
        // some comment
        let a = 8
        let b = List.length contentBefore
        a + b)
"""

[<Test>]
let ``indent when identifier is smaller than ident size`` () =
    formatSourceString
        false
        """
foo (fun a ->
                let b = 8
                b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
foo (fun a ->
    let b = 8
    b)
"""

[<Test>]
let ``short ident in nested let binding`` () =
    formatSourceString
        false
        """let a =
    foo (fun a ->
                let b = 8
                b)
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let a =
  foo (fun a ->
    let b = 8
    b)
"""

[<Test>]
let ``longer ident in nested let binding`` () =
    formatSourceString
        false
        """let a =
    foobar (fun a ->
                let b = 8
                b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    foobar (fun a ->
        let b = 8
        b)
"""

[<Test>]
let ``multiple braces should add indent`` () =
    formatSourceString
        false
        """((((fun () ->
    printfn "meh"
    ()))))
"""
        config
    |> prepend newline
    |> should
        equal
        """
((((fun () ->
    printfn "meh"
    ()))))
"""

[<Test>]
let ``add space after chained ident, 676`` () =
    formatSourceString false """let foo = Foo(fun () -> Foo.Create x).Value""" config
    |> prepend newline
    |> should
        equal
        """
let foo = Foo(fun () -> Foo.Create x).Value
"""

[<Test>]
let ``line comment after lambda should not necessary make it multiline`` () =
    formatSourceString
        false
        """let a = fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
"""
        { config with
            MaxFunctionBindingWidth = 150 }
    |> prepend newline
    |> should
        equal
        """
let a = fun _ -> div [] [] // React.lazy is not compatible with SSR, so just use an empty div
"""

[<Test>]
let ``multiline let binding in lambda`` () =
    formatSourceString
        false
        """
CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
            let connectionString =
                if hostedService then RoleEnvironment.GetConfigurationSettingValue(configName)
                else ConfigurationManager.ConnectionStrings.[configName].ConnectionString
            configSettingPublisher.Invoke(connectionString) |> ignore)
"""
        { config with
            MaxDotGetExpressionWidth = 50
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
    let connectionString =
        if hostedService then
            RoleEnvironment.GetConfigurationSettingValue(configName)
        else
            ConfigurationManager.ConnectionStrings.[configName].ConnectionString

    configSettingPublisher.Invoke(connectionString)
    |> ignore)
"""

[<Test>]
let ``line comment after arrow should not introduce additional newline, 772`` () =
    formatSourceString
        false
        """let genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        match mf with
        | MFOverride _ ->
            (fun (ctx: Context) -> // trying to get AST trivia

                ctx.Trivia
                |> List.tryFind (fun { Type = t; Range = r } -> // trying to get token trivia

                    match t with
                    | MainNode "SynMemberDefn.Member" -> RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                    | Token { TokenInfo = { TokenName = "MEMBER" } } -> r.StartLine = rangeOfBindingAndRhs.StartLine

                    | _ -> false)
                |> Option.defaultValue (!- "override ")
                <| ctx)
        <| ctx
"""
        config
    |> prepend newline
    |> should
        equal
        """
let genMemberFlagsForMemberBinding astContext (mf: MemberFlags) (rangeOfBindingAndRhs: range) =
    fun ctx ->
        match mf with
        | MFOverride _ ->
            (fun (ctx: Context) -> // trying to get AST trivia

                ctx.Trivia
                |> List.tryFind (fun { Type = t; Range = r } -> // trying to get token trivia

                    match t with
                    | MainNode "SynMemberDefn.Member" -> RangeHelpers.``range contains`` r rangeOfBindingAndRhs

                    | Token { TokenInfo = { TokenName = "MEMBER" } } -> r.StartLine = rangeOfBindingAndRhs.StartLine

                    | _ -> false)
                |> Option.defaultValue (!- "override ")
                <| ctx)
        <| ctx
"""

[<Test>]
let ``line comment after arrow should not introduce extra newline`` () =
    formatSourceString
        false
        """
List.tryFind (fun { Type = t; Range = r } -> // foo
                    let a = 8
                    a + 9)
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.tryFind (fun { Type = t; Range = r } -> // foo
    let a = 8
    a + 9)
"""

[<Test>]
let ``lambda body should be indented far enough, 870`` () =
    formatSourceString
        false
        """
let projectIntoMap projection =
    fun state eventEnvelope ->
      state
      |> Map.tryFind eventEnvelope.Metadata.Source
      |> Option.defaultValue projection.Init
      |> fun projectionState -> eventEnvelope.Event |> projection.Update projectionState
      |> fun newState -> state |> Map.add eventEnvelope.Metadata.Source newState

let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
         eventEnvelope.Event
         |> projection.Update projectionState
    |> fun newState ->
         state
         |> Map.add eventEnvelope.Metadata.Source newState
"""
        { config with
            IndentSize = 2
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeColon = true
            SpaceAfterComma = false
            SpaceAroundDelimiter = false
            MaxInfixOperatorExpression = 40
            MaxFunctionBindingWidth = 60
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
        eventEnvelope.Event
        |> projection.Update projectionState
    |> fun newState ->
        state
        |> Map.add eventEnvelope.Metadata.Source newState

let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState ->
        eventEnvelope.Event
        |> projection.Update projectionState
    |> fun newState ->
        state
        |> Map.add eventEnvelope.Metadata.Source newState
"""

[<Test>]
let ``lambda body should not get an additional indent when the indent_size is large enough`` () =
    formatSourceString
        false
        """
let projectIntoMap projection =
  fun state eventEnvelope ->
    state
    |> Map.tryFind eventEnvelope.Metadata.Source
    |> Option.defaultValue projection.Init
    |> fun projectionState -> eventEnvelope.Event |> projection.Update projectionState
    |> fun newState -> state |> Map.add eventEnvelope.Metadata.Source newState
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let projectIntoMap projection =
    fun state eventEnvelope ->
        state
        |> Map.tryFind eventEnvelope.Metadata.Source
        |> Option.defaultValue projection.Init
        |> fun projectionState ->
            eventEnvelope.Event
            |> projection.Update projectionState
        |> fun newState ->
            state
            |> Map.add
                eventEnvelope.Metadata.Source
                newState
"""

[<Test>]
let ``don't duplicate new line before LongIdentSet`` () =
    formatSourceString
        false
        """
        let options =
            jsOptions<Vis.Options> (fun o ->
                let layout =
                    match opts.Layout with
                    | Graph.Free -> createObj []
                    | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
                    | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

                o.layout <- Some layout)
"""
        { config with
            MaxValueBindingWidth = 50
            MaxFunctionBindingWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
let options =
    jsOptions<Vis.Options> (fun o ->
        let layout =
            match opts.Layout with
            | Graph.Free -> createObj []
            | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
            | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

        o.layout <- Some layout)
"""

[<Test>]
let ``don't print unrelated trivia after closing parenthesis of lambda, 1084`` () =
    formatSourceString
        false
        """
let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
  allLines
  |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
  |> List.fold (fun (state, tokens) (line, lineNumber) ->
      let tokenizer = sourceTokenizer.CreateLineTokenizer(line)
      let nextState, tokensOfLine =
          tokenizeLine tokenizer allLines state lineNumber []

      let allTokens = List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order
      (nextState, allTokens)
  ) (state, []) // empty tokens to start with
  |> snd // ignore the state
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private tokenizeLines (sourceTokenizer: FSharpSourceTokenizer) allLines state =
    allLines
    |> List.mapi (fun index line -> line, (index + 1)) // line number is needed in tokenizeLine
    |> List.fold
        (fun (state, tokens) (line, lineNumber) ->
            let tokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let nextState, tokensOfLine = tokenizeLine tokenizer allLines state lineNumber []

            let allTokens = List.append tokens (List.rev tokensOfLine) // tokens of line are add in reversed order
            (nextState, allTokens))
        (state, []) // empty tokens to start with
    |> snd // ignore the state
"""

[<Test>]
let ``trivia before closing parenthesis of desugared lambda, 1146`` () =
    formatSourceString
        false
        """
Target.create "Install" (fun _ ->
    Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
    // Paket restore will already happen when the build.fsx dependencies are restored
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Target.create "Install" (fun _ -> Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
// Paket restore will already happen when the build.fsx dependencies are restored
)
"""

[<Test>]
let ``trivia before closing parenthesis of lambda`` () =
    formatSourceString
        false
        """
Target.create "Install" (fun x ->
    Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
    // Paket restore will already happen when the build.fsx dependencies are restored
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Target.create "Install" (fun x -> Yarn.install (fun o -> { o with WorkingDirectory = clientDir })
// Paket restore will already happen when the build.fsx dependencies are restored
)
"""

[<Test>]
let ``function call with two lambda arguments, 1164`` () =
    formatSourceString
        false
        """
let init =
  addDateTimeConverter
    (fun dt -> Date(dt.Year, dt.Month, dt.Day))
    (fun (Date (y, m, d)) ->
      System.DateTime(y, m, d))
"""
        { config with MaxLineLength = 85 }
    |> prepend newline
    |> should
        equal
        """
let init =
    addDateTimeConverter
        (fun dt -> Date(dt.Year, dt.Month, dt.Day))
        (fun (Date(y, m, d)) -> System.DateTime(y, m, d))
"""

[<Test>]
let ``function call with two lambdas and three other arguments`` () =
    formatSourceString
        false
        """
SettingControls.toggleButton (fun _ ->
    UpdateOption(key, MultilineFormatterTypeOption(o, key, "character_width"))
    |> dispatch) (fun _ ->
    UpdateOption(key, MultilineFormatterTypeOption(o, key, "number_of_items"))
    |> dispatch) "CharacterWidth" "NumberOfItems" key (v = "character_width")
"""
        config
    |> prepend newline
    |> should
        equal
        """
SettingControls.toggleButton
    (fun _ ->
        UpdateOption(key, MultilineFormatterTypeOption(o, key, "character_width"))
        |> dispatch)
    (fun _ ->
        UpdateOption(key, MultilineFormatterTypeOption(o, key, "number_of_items"))
        |> dispatch)
    "CharacterWidth"
    "NumberOfItems"
    key
    (v = "character_width")
"""

[<Test>]
let ``lambda should be on the next line, 1201`` () =
    formatSourceString
        false
        """
let printListWithOffset a list1 =
    List.iter
        (fun elem -> printfn "%d" (a + elem))
        list1

// OK if lambda body is long enough
let printListWithOffset a list1 =
    List.iter
        (fun elem ->
            // OK if lambda body is long enough
            printfn "%d" (a + elem))
        list1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let printListWithOffset a list1 =
    List.iter (fun elem -> printfn "%d" (a + elem)) list1

// OK if lambda body is long enough
let printListWithOffset a list1 =
    List.iter
        (fun elem ->
            // OK if lambda body is long enough
            printfn "%d" (a + elem))
        list1
"""

[<Test>]
let ``Thoth.Json decoder, 685`` () =
    formatSourceString
        false
        """
Decode.map3 (fun aggregateId event commitPayload ->
    match commitPayload with
    | Some payload ->
        Some
            { AggregateId = AggregateId aggregateId
              Event = event
              Payload = payload }
    | None -> None) (Decode.field "aggregate_id" Decode.string) (Decode.field "event" Decode.string) decodePayload
"""
        config
    |> prepend newline
    |> should
        equal
        """
Decode.map3
    (fun aggregateId event commitPayload ->
        match commitPayload with
        | Some payload ->
            Some
                { AggregateId = AggregateId aggregateId
                  Event = event
                  Payload = payload }
        | None -> None)
    (Decode.field "aggregate_id" Decode.string)
    (Decode.field "event" Decode.string)
    decodePayload
"""

[<Test>]
let ``add extra indent in fluent api, 970`` () =
    formatSourceString
        false
        """
  services.AddAuthentication(fun options ->
          options.DefaultScheme <- "Cookies"
          options.DefaultChallengeScheme <- "oidc").AddCookie("Cookies")
          .AddOpenIdConnect(fun options ->
          options.Authority <- "http://localhost:7001"
          options.ClientId <- "mvc"
          options.ClientSecret <- "secret"
          options.ResponseType <- "code"
          options.SaveTokens <- true)
"""
        config
    |> prepend newline
    |> should
        equal
        """
services
    .AddAuthentication(fun options ->
        options.DefaultScheme <- "Cookies"
        options.DefaultChallengeScheme <- "oidc")
    .AddCookie("Cookies")
    .AddOpenIdConnect(fun options ->
        options.Authority <- "http://localhost:7001"
        options.ClientId <- "mvc"
        options.ClientSecret <- "secret"
        options.ResponseType <- "code"
        options.SaveTokens <- true)
"""

[<Test>]
let ``correctly indent nested lambda inside fluent api`` () =
    formatSourceString
        false
        """
services.AddHttpsRedirection(Action<HttpsRedirectionOptions>(fun options ->
    // meh
    options.HttpsPort <- Nullable(7002)
)) |> ignore
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
services.AddHttpsRedirection(
    Action<HttpsRedirectionOptions>(fun options ->
        // meh
        options.HttpsPort <- Nullable(7002))
)
|> ignore
"""

[<Test>]
let ``comment between opening parenthesis and lambda, 1190`` () =
    formatSourceString
        false
        """
(
    (* comment before gets swallowed *)
    fun x -> x * 42
)

(
    fun x -> x * 42
    (* comment after is OK *)
)

(   (* comment on first line is OK too *)
    fun x -> x * 42
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(
(* comment before gets swallowed *)
fun x -> x * 42)

(fun x -> x * 42
(* comment after is OK *)
)

( (* comment on first line is OK too *) fun x -> x * 42)
"""

[<Test>]
let ``desugared union case, 1631`` () =
    formatSourceString
        false
        """
      col
                        (fun (ArgInfo (ats, so, isOpt), t) -> sepNone)
"""
        config
    |> prepend newline
    |> should
        equal
        """
col (fun (ArgInfo(ats, so, isOpt), t) -> sepNone)
"""

[<Test>]
let ``two wild args`` () =
    formatSourceString
        false
        """
fun _ _ -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun _ _ -> ()
"""

[<Test>]
let ``lambda argument in multiline function application, 1028`` () =
    formatSourceString
        false
        """
module Lifecycle =


  let init config =
    async {
      cfg <- config
      do!
        MassTransit.init
          cfg.LoggerFactory cfg.AzureServiceBusConnStr cfg.QueueName cfg.LoggerFactory
          (fun reg ->
            reg.Consume User.handleUserInitiatedRegistration
            reg.Consume User.handleUserUpdated
            reg.Consume User.handleGetSessionUserIdRequest
          )
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Lifecycle =


    let init config =
        async {
            cfg <- config

            do!
                MassTransit.init
                    cfg.LoggerFactory
                    cfg.AzureServiceBusConnStr
                    cfg.QueueName
                    cfg.LoggerFactory
                    (fun reg ->
                        reg.Consume User.handleUserInitiatedRegistration
                        reg.Consume User.handleUserUpdated
                        reg.Consume User.handleGetSessionUserIdRequest)
        }
"""

[<Test>]
let ``return lambda from lambda, 1782`` () =
    formatSourceString
        false
        """
let x =
    fun _ ->
        fun _ -> "hello"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = fun _ -> fun _ -> "hello"
"""

[<Test>]
let ``wild card parameters in lambda, 1789`` () =
    formatSourceString
        false
        """
let elifs =
    es
    |> List.collect (fun (e1, e2, _, _, _) -> [ visit e1; visit e2 ])
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let elifs =
    es
    |> List.collect (fun (e1, e2, _, _, _) -> [ visit e1; visit e2 ])
"""

[<Test>]
let ``leading and trailing wild card parameters in lambda`` () =
    formatSourceString
        false
        """
List.map (fun (_, _, _, _, body, _) -> visit body) andBangs
"""
        config
    |> prepend newline
    |> should
        equal
        """
List.map (fun (_, _, _, _, body, _) -> visit body) andBangs
"""

[<Test>]
let ``multiple parameters with wild cards, 1806`` () =
    formatSourceString
        false
        """
module Foo =
    let bar () =
        {
            Foo =
                blah
                |> Struct.map (fun _ (a, _, _) -> filterBackings a)
        }
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar () =
        { Foo =
            blah
            |> Struct.map (fun _ (a, _, _) -> filterBackings a) }
"""

[<Test>]
let ``multiline SynExpr.MatchLambda`` () =
    formatSourceString
        false
        """
module Foo =
    let bar =
        []
        |> List.choose
            (function
             | _ -> "")
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar =
        []
        |> List.choose (function
            | _ -> "")
"""

[<Test>]
let ``long function application ending in with lambda argument`` () =
    formatSourceString
        false
        """
let foobar =
    someFunctionName aFirstLongArgument aSecondLongArgument aThirdLongArgument aFourthLongArgument (fun finallyThatLambdaArgument ->
        aFirstLongArgument +  aSecondLongArgument -  aThirdLongArgument -  aFourthLongArgument + finallyThatLambdaArgument)

let somethingElse = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foobar =
    someFunctionName
        aFirstLongArgument
        aSecondLongArgument
        aThirdLongArgument
        aFourthLongArgument
        (fun finallyThatLambdaArgument ->
            aFirstLongArgument + aSecondLongArgument
            - aThirdLongArgument
            - aFourthLongArgument
            + finallyThatLambdaArgument)

let somethingElse = ()
"""

[<Test>]
let ``multiline non lambda argument`` () =
    formatSourceString
        false
        """
let argExpr =
    col sepNln es (fun e ->
        let genLambda
            (pats: Context -> Context)
            (bodyExpr: SynExpr)
            (lpr: Range)
            (rpr: Range option)
            (arrowRange: Range)
            (pr: Range)
            : Context -> Context =
            leadingExpressionIsMultiline (sepOpenTFor lpr -- "fun "
                                            +> pats
                                            +> genArrowWithTrivia
                                                (genExprKeepIndentInBranch astContext bodyExpr)
                                                arrowRange) (fun isMultiline ->
                onlyIf isMultiline sepNln
                +> sepCloseTFor rpr e.Range)
            |> genTriviaFor SynExpr_Paren pr
        ()
    )
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let argExpr =
    col sepNln es (fun e ->
        let genLambda
            (pats: Context -> Context)
            (bodyExpr: SynExpr)
            (lpr: Range)
            (rpr: Range option)
            (arrowRange: Range)
            (pr: Range)
            : Context -> Context =
            leadingExpressionIsMultiline
                (sepOpenTFor lpr -- "fun "
                 +> pats
                 +> genArrowWithTrivia (genExprKeepIndentInBranch astContext bodyExpr) arrowRange)
                (fun isMultiline ->
                    onlyIf isMultiline sepNln
                    +> sepCloseTFor rpr e.Range)
            |> genTriviaFor SynExpr_Paren pr

        ())
"""

[<Test>]
let ``multiline non lambda argument, match lambda`` () =
    formatSourceString
        false
        """
leadingExpressionIsMultiline (sepOpenTFor lpr -- "fun "
                                +> pats
                                +> genArrowWithTrivia
                                    (genExprKeepIndentInBranch astContext bodyExpr)
                                    arrowRange) (function | Ok _ -> true | Error _ -> false)
"""
        config
    |> prepend newline
    |> should
        equal
        """
leadingExpressionIsMultiline
    (sepOpenTFor lpr -- "fun "
     +> pats
     +> genArrowWithTrivia (genExprKeepIndentInBranch astContext bodyExpr) arrowRange)
    (function
     | Ok _ -> true
     | Error _ -> false)
"""

[<Test>]
let ``multiline lambda argument, 1922`` () =
    formatSourceString
        false
        """
let g =
    Array.groupBy
        (fun { partNumber = p
               revisionNumber = r
               processName = pn } -> p, r, pn)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let g =
    Array.groupBy
        (fun
            { partNumber = p
              revisionNumber = r
              processName = pn } -> p, r, pn)
"""

[<Test>]
let ``comment after arrow is preserved, 1870`` () =
    formatSourceString
        false
        """
fun a ->   // foo
    a
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun a -> // foo
    a
"""

[<Test>]
let ``parenthesis function call with lambda argument, 2015`` () =
    formatSourceString
        false
        """
(if true then foo else goo) (fun _ -> 42)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(if true then foo else goo) (fun _ -> 42)
"""

[<Test>]
let ``parenthesis function call with long lambda argument`` () =
    formatSourceString
        false
        """
(if true then foo else goo) (fun _ ->
                                                        // comment
                                                        42)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(if true then foo else goo) (fun _ ->
    // comment
    42)
"""

[<Test>]
let ``function expression and argument expression with parenthesis, 1998`` () =
    formatSourceString
        false
        """
(SomeModule.doSomething << SomeModule.doSomethingElse) (fun x -> x)
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
(SomeModule.doSomething
 << SomeModule.doSomethingElse)
    (fun x -> x)
"""

[<Test>]
let ``piped lambda with if-then-else, 2196`` () =
    formatSourceString
        false
        """
let dayOfWeekToNum (d: DayOfWeek) =
    int d
    |> fun x -> if x = 0 then 7 else x
    |> DayNum
"""
        { config with
            MaxInfixOperatorExpression = 45 }
    |> prepend newline
    |> should
        equal
        """
let dayOfWeekToNum (d: DayOfWeek) =
    int d
    |> fun x -> if x = 0 then 7 else x
    |> DayNum
"""

[<Test>]
let ``piped lambda with if-then-else, short, 2196`` () =
    formatSourceString
        false
        """
let foo () =
    f()
    |> fun x -> if x then 1 else 2
    |> g
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo () =
    f () |> (fun x -> if x then 1 else 2) |> g
"""

[<Test>]
let ``don't add space before paren lambda argument, 2041`` () =
    formatSourceString
        false
        """
Task.Run<CommandResult>(fun () ->
    // long
    // comment
    task)
|> ignore<Task<CommandResult>>

Task.Run<CommandResult> (task)
|> ignore<Task<CommandResult>>
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
Task.Run<CommandResult>(fun () ->
    // long
    // comment
    task)
|> ignore<Task<CommandResult>>

Task.Run<CommandResult>(task)
|> ignore<Task<CommandResult>>
"""

[<Test>]
let ``don't add space before linq lambda and idempotent, 2231`` () =
    formatSourceString
        false
        """
open System.Linq

type Item() =
    member val ValidFrom = DateTime.MinValue
    member val Value = 23.42m

let items = [ Item(); Item(); Item() ]

let firstOrDef = items.FirstOrDefault(fun x ->
    x.ValidFrom <= DateTime.Now || x.ValidFrom > DateTime.Now).Value
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
open System.Linq

type Item() =
    member val ValidFrom = DateTime.MinValue
    member val Value = 23.42m

let items = [ Item(); Item(); Item() ]

let firstOrDef =
    items
        .FirstOrDefault(fun x ->
            x.ValidFrom <= DateTime.Now
            || x.ValidFrom > DateTime.Now)
        .Value
"""

[<Test>]
let ``lambda with long list of arguments`` () =
    formatSourceString
        false
        """
fun (a0: int) (a1: int) (a2: int) (a3: int) (a4: int) (a5: int) (a6: int) (a7: int) (a8: int) (a9: int) (a10: int) (a11: int) (a12: int) (a13: int) (a14: int) (a15: int) (a16: int) (a17: int) (a18: int) (a19: int) (a20: int) (a21: int) (a22: int) (a23: int) (a24: int) (a25: int) (a26: int) (a27: int) (a28: int) (a29: int) (a30: int) (a31: int) (a32: int) (a33: int) (a34: int) (a35: int) (a36: int) (a37: int) (a38: int) (a39: int) (a40: int) (a41: int) (a42: int) (a43: int) (a44: int) (a45: int) (a46: int) (a47: int) (a48: int) (a49: int) (a50: int) (a51: int) (a52: int) (a53: int) (a54: int) (a55: int) (a56: int) (a57: int) (a58: int) (a59: int) (a60: int) (a61: int) (a62: int) (a63: int) (a64: int) (a65: int) (a66: int) (a67: int) (a68: int) (a69: int) (a70: int) (a71: int) (a72: int) (a73: int) (a74: int) (a75: int) (a76: int) (a77: int) (a78: int) (a79: int) (a80: int) (a81: int) (a82: int) (a83: int) (a84: int) (a85: int) (a86: int) (a87: int) (a88: int) (a89: int) (a90: int) (a91: int) (a92: int) (a93: int) (a94: int) (a95: int) (a96: int) (a97: int) (a98: int) (a99: int) -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun
    (a0: int)
    (a1: int)
    (a2: int)
    (a3: int)
    (a4: int)
    (a5: int)
    (a6: int)
    (a7: int)
    (a8: int)
    (a9: int)
    (a10: int)
    (a11: int)
    (a12: int)
    (a13: int)
    (a14: int)
    (a15: int)
    (a16: int)
    (a17: int)
    (a18: int)
    (a19: int)
    (a20: int)
    (a21: int)
    (a22: int)
    (a23: int)
    (a24: int)
    (a25: int)
    (a26: int)
    (a27: int)
    (a28: int)
    (a29: int)
    (a30: int)
    (a31: int)
    (a32: int)
    (a33: int)
    (a34: int)
    (a35: int)
    (a36: int)
    (a37: int)
    (a38: int)
    (a39: int)
    (a40: int)
    (a41: int)
    (a42: int)
    (a43: int)
    (a44: int)
    (a45: int)
    (a46: int)
    (a47: int)
    (a48: int)
    (a49: int)
    (a50: int)
    (a51: int)
    (a52: int)
    (a53: int)
    (a54: int)
    (a55: int)
    (a56: int)
    (a57: int)
    (a58: int)
    (a59: int)
    (a60: int)
    (a61: int)
    (a62: int)
    (a63: int)
    (a64: int)
    (a65: int)
    (a66: int)
    (a67: int)
    (a68: int)
    (a69: int)
    (a70: int)
    (a71: int)
    (a72: int)
    (a73: int)
    (a74: int)
    (a75: int)
    (a76: int)
    (a77: int)
    (a78: int)
    (a79: int)
    (a80: int)
    (a81: int)
    (a82: int)
    (a83: int)
    (a84: int)
    (a85: int)
    (a86: int)
    (a87: int)
    (a88: int)
    (a89: int)
    (a90: int)
    (a91: int)
    (a92: int)
    (a93: int)
    (a94: int)
    (a95: int)
    (a96: int)
    (a97: int)
    (a98: int)
    (a99: int) -> ()
"""

[<Test>]
let ``lambda with long list of arguments, wrapped in parentheses`` () =
    formatSourceString
        false
        """
(fun (a0: int) (a1: int) (a2: int) (a3: int) (a4: int) (a5: int) (a6: int) (a7: int) (a8: int) (a9: int) (a10: int) (a11: int) (a12: int) (a13: int) (a14: int) (a15: int) (a16: int) (a17: int) (a18: int) (a19: int) (a20: int) -> ())
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun
    (a0: int)
    (a1: int)
    (a2: int)
    (a3: int)
    (a4: int)
    (a5: int)
    (a6: int)
    (a7: int)
    (a8: int)
    (a9: int)
    (a10: int)
    (a11: int)
    (a12: int)
    (a13: int)
    (a14: int)
    (a15: int)
    (a16: int)
    (a17: int)
    (a18: int)
    (a19: int)
    (a20: int) -> ())
"""

[<Test>]
let ``indent closing parenthesis far enough in lambda application, 1299`` () =
    formatSourceString
        false
        "
let _ =
    [] |> List.map (fun _ -> @\"a
b\"     )
       |> List.length
"
        config
    |> prepend newline
    |> should
        equal
        "
let _ =
    []
    |> List.map (fun _ ->
        @\"a
b\"  )
    |> List.length
"

[<Test>]
let ``indent closing parenthesis far enough in multiline lambda application`` () =
    formatSourceString
        false
        "
let _ =
    List.maaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaap (fun _ -> @\"a
b\"     )
       |> List.length
"
        { config with MaxLineLength = 40 }
    |> prepend newline
    |> should
        equal
        "
let _ =
    List.maaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaap
        (fun _ ->
            @\"a
b\"      )
    |> List.length
"

[<Test>]
let ``lambda with generic argument in function identifier, 2699`` () =
    formatSourceString
        false
        """
MailboxProcessor<string>.Start
    (fun inbox ->
        async {
            while true do
                let! msg = inbox.Receive()
                do! sw.WriteLineAsync(msg) |> Async.AwaitTask
        })
"""
        config
    |> prepend newline
    |> should
        equal
        """
MailboxProcessor<string>.Start(fun inbox ->
    async {
        while true do
            let! msg = inbox.Receive()
            do! sw.WriteLineAsync(msg) |> Async.AwaitTask
    })
"""
