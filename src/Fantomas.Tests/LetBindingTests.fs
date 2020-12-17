module Fantomas.Tests.LetBindingTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``let in should be preserved`` () =
    formatSourceString false "let x = 1 in ()" config
    |> should equal """let x = 1 in ()
"""

[<Test>]
let ``multiple let in lines, should keep in`` () =
    let codeSnippet = """
let f () =
  let x = 1 in   // the "in" keyword is available in F#
    let y = 2 in
      x + y
"""

    formatSourceString false codeSnippet config
    |> should equal """let f () =
    let x = 1 in // the "in" keyword is available in F#
    let y = 2 in
    x + y
"""

[<Test>]
let ``multiple let in lines, should keep in, block comment`` () =
    let codeSnippet = """
let f () =
  let x = 1 in   (* the "in" keyword is available in F# *)
    let y = 2 in
      x + y
"""

    formatSourceString
        false
        codeSnippet
        ({ config with
               MaxValueBindingWidth = 50 })
    |> should equal """let f () =
    let x = 1 in (* the "in" keyword is available in F# *)
    let y = 2 in
    x + y
"""

[<Test>]
let ``multiline let in, should keep in`` () =
    let codeSnippet = """
let f () =
  let x = 1 in if longIdentifierThatWillForceThisConstructToBeMultiline
               then x
               else x
"""

    formatSourceString false codeSnippet config
    |> prepend newline
    |> should equal """
let f () =
    let x = 1 in

    if longIdentifierThatWillForceThisConstructToBeMultiline then
        x
    else
        x
"""

[<Test>]
let ``multiline let in, should remove in 2`` () =
    let codeSnippet = """
let f () =
  let x = 1 in (while true do ()
                x)
"""

    formatSourceString false codeSnippet config
    |> prepend newline
    |> should equal """
let f () =
    let x = 1 in

    (while true do
        ()

     x)
"""

[<Test>]
let ``DotGet on newline should be indented far enough`` () =
    formatSourceString false """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""
        ({ config with
               MaxValueBindingWidth = 70 })
    |> prepend newline
    |> should equal """
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
"""

[<Test>]
let ``DotGet on newline after empty string should be indented far enough`` () =
    formatSourceString false """
let x =
    [| 1..2 |]
    |> Array.mapi (fun _ _ ->
        let num =
            ""
                .PadLeft(9)
        num)
"""  config
    |> prepend newline
    |> should equal """
let x =
    [| 1 .. 2 |]
    |> Array.mapi
        (fun _ _ ->
            let num = "".PadLeft(9)
            num)
"""

[<Test>]
let ``newlines between let bindings should preserved`` () =
    formatSourceString false """
let a = 42



let b = "meh"
"""  config
    |> should equal """let a = 42



let b = "meh"
"""

[<Test>]
let ``Raw method names with `/` `` () =
    formatSourceString false "let ``/ operator combines paths`` = x" config
    |> should equal """let ``/ operator combines paths`` = x
"""

[<Test>]
let ``newline before let inside let should not be duplicated`` () =
    formatSourceString false """namespace ReactStrap

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

[<RequireQualifiedAccess>]
module Card =
    type CardProps =
        | Tag of U2<string, obj>
        | Inverse of bool
        | Outline of bool
        | Color of Common.Color
        | Body of bool
        | Custom of IHTMLProp list

    let card (props: CardProps seq) (elems: ReactElement seq): ReactElement =
        let customProps =
            props
            |> Seq.collect (function
                | Custom props -> props
                | _ -> List.empty)
            |> keyValueList CaseRules.LowerFirst

        let typeProps =
            props
            |> Seq.choose (function
                | Custom _ -> None
                | prop -> Some prop)
            |> keyValueList CaseRules.LowerFirst

        let props = JS.Object.assign (createEmpty, customProps, typeProps)
        ofImport "Card" "reactstrap" props elems""" config
    |> should equal """namespace ReactStrap

open Fable.Core
open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props

[<RequireQualifiedAccess>]
module Card =
    type CardProps =
        | Tag of U2<string, obj>
        | Inverse of bool
        | Outline of bool
        | Color of Common.Color
        | Body of bool
        | Custom of IHTMLProp list

    let card (props: CardProps seq) (elems: ReactElement seq): ReactElement =
        let customProps =
            props
            |> Seq.collect
                (function
                | Custom props -> props
                | _ -> List.empty)
            |> keyValueList CaseRules.LowerFirst

        let typeProps =
            props
            |> Seq.choose
                (function
                | Custom _ -> None
                | prop -> Some prop)
            |> keyValueList CaseRules.LowerFirst

        let props =
            JS.Object.assign (createEmpty, customProps, typeProps)

        ofImport "Card" "reactstrap" props elems
"""

[<Test>]
let ``newlines inside let binding should be not duplicated`` () =
    formatSourceString false """let foo =
    let next _ =
        if not animating then activeIndex.update ((activeIndex.current + 1) % itemLength)

    let prev _ =
        if not animating then activeIndex.update ((activeIndex.current + itemLength - 1) % itemLength)

    ()
"""
        ({ config with
               MaxInfixOperatorExpression = 60 })
    |> should equal """let foo =
    let next _ =
        if not animating
        then activeIndex.update ((activeIndex.current + 1) % itemLength)

    let prev _ =
        if not animating
        then activeIndex.update ((activeIndex.current + itemLength - 1) % itemLength)

    ()
"""

[<Test>]
let ``inner let binding should not add additional newline, #475`` () =
    formatSourceString false "module Test =
    let testFunc() =
        let someObject =
            someStaticObject.Create(
                ((fun o ->
                    o.SomeProperty <- \"\"
                    o.Role <- \"Has to be at least two properties\")))

        /// Comment can't be removed to reproduce bug
        let someOtherValue = \"\"

        someObject.someFunc \"can't remove any of this stuff\"
        someMutableProperty <- \"not even this\"" config
    |> prepend newline
    |> should equal "
module Test =
    let testFunc () =
        let someObject =
            someStaticObject.Create(
                ((fun o ->
                    o.SomeProperty <- \"\"
                    o.Role <- \"Has to be at least two properties\"))
            )

        /// Comment can't be removed to reproduce bug
        let someOtherValue = \"\"

        someObject.someFunc \"can't remove any of this stuff\"
        someMutableProperty <- \"not even this\"
"

[<Test>]
let ``don't add significant spacing after let binding, #478`` () =
    formatSourceString false """let someFun someReallyLoooooooooooooooongValue =
    let someValue = someReallyLoooooooooooooooongValue

    someOtherFun 1 3

    someOtherOtherFun 2 4
"""  config
    |> prepend newline
    |> should equal """
let someFun someReallyLoooooooooooooooongValue =
    let someValue = someReallyLoooooooooooooooongValue

    someOtherFun 1 3

    someOtherOtherFun 2 4
"""

[<Test>]
let ``should keep space before :`` () =
    formatSourceString false "let refl<'a> : Teq<'a, 'a> = Teq(id,   id)" config
    |> fun formatted -> formatSourceString false formatted config
    |> should equal "let refl<'a> : Teq<'a, 'a> = Teq(id, id)
"

[<Test>]
let ``newline trivia before simple sequence doesn't force remaining to get offset by last expression column index, 513`` () =
    formatSourceString false """let a() =
    let q = 1

    q
    b
"""  config
    |> prepend newline
    |> should equal """
let a () =
    let q = 1

    q
    b
"""

[<Test>]
let ``comment trivia before simple sequence doesn't force remaining to get offset by last expression column index, 513`` () =
    formatSourceString false """let a() =
    let q = 1
    // comment
    q
    b
"""  config
    |> should equal """let a () =
    let q = 1
    // comment
    q
    b
"""

[<Test>]
let ``no extra newline should be added between IfThenElse within Sequential, 588`` () =
    shouldNotChangeAfterFormat """
let x =
    if true then printfn "a"
    elif true then printfn "b"

    if true then 1 else 0
"""

[<Test>]
let ``line comment before return type info should indent before colon, 565`` () =
    formatSourceString false """module Bar =
  let f a
    // foo
    : int
    =
    0
"""
        ({ config with
               SpaceAfterComma = false
               SpaceAfterSemicolon = false
               SpaceAroundDelimiter = false })
    |> prepend newline
    |> should equal """
module Bar =
    let f a
          // foo
          : int =
        0
"""

[<Test>]
let ``line comment before return type with AlignFunctionSignatureToIndentation`` () =
    formatSourceString false """
  let functionName a b c
    // foo
    : int
    =
    0
"""
        { config with
              AlignFunctionSignatureToIndentation = true }
    |> prepend newline
    |> should equal """
let functionName
    a
    b
    c
    // foo
    : int
    =
    0
"""

[<Test>]
let ``has symbol in signature requires paren, 564`` () =
    formatSourceString false """module Bar =
  let foo (_ : #(int seq)) = 1
  let meh (_: #seq<int>) = 2
  let evenMoreMeh (_: #seq<int>) : int = 2
"""
        ({ config with
               SpaceAfterComma = false
               SpaceAfterSemicolon = false
               SpaceAroundDelimiter = false
               SpaceBeforeParameter = false })
    |> prepend newline
    |> should equal """
module Bar =
    let foo(_: #(int seq)) = 1
    let meh(_: #seq<int>) = 2
    let evenMoreMeh(_: #seq<int>): int = 2
"""

[<Test>]
let ``only add one space between idents in app`` () =
    formatSourceString false "let validatorResult = validator input" config
    |> should equal "let validatorResult = validator input
"

[<Test>]
let ``multiline let binding, should be multiline based on expression, not AST composition`` () =
    formatSourceString false """
let foo a =
    let b = a +   7
    b
"""  config
    |> prepend newline
    |> should equal """
let foo a =
    let b = a + 7
    b
"""

[<Test>]
let ``multiline let binding with type signature should be multiline based on expression, not AST composition`` () =
    formatSourceString false """
let foo (a: int ) (b:  string):string =
    let c = a.ToString() + b
    sprintf "result: %s" c
"""  config
    |> prepend newline
    |> should equal """
let foo (a: int) (b: string): string =
    let c = a.ToString() + b
    sprintf "result: %s" c
"""

[<Test>]
let ``multiline inner let binding in nested module`` () =
    formatSourceString false """let SetQuartzLoggingFunction f =
        let loggerFunction level (func: Func<string>) exc parameters =
            let wrappedFunction =
                Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func
            let wrappedException = Helpers.nullValuesToOptions id exc
            f level wrappedFunction wrappedException (parameters |> List.ofArray)

        LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))
"""  config
    |> prepend newline
    |> should equal """
let SetQuartzLoggingFunction f =
    let loggerFunction level (func: Func<string>) exc parameters =
        let wrappedFunction =
            Helpers.nullValuesToOptions (fun (x: Func<string>) -> (fun () -> x.Invoke())) func

        let wrappedException = Helpers.nullValuesToOptions id exc
        f level wrappedFunction wrappedException (parameters |> List.ofArray)

    LogProvider.SetCurrentLogProvider(QuartzLoggerWrapper(loggerFunction))
"""

[<Test>]
let ``determine lower or uppercase in paren, 753`` () =
    formatSourceString false """let genSigModuleDeclList astContext node =
    match node with
    | [x] -> genSigModuleDecl astContext x

    | SigOpenL(xs, ys) ->
        let sepXsAndYs =
            match List.tryHead ys with
            | Some hs ->
                let attrs = getRangesFromAttributesFromSynModuleSigDeclaration hs
                sepNln +> sepNlnConsideringTriviaContentBeforeWithAttributes hs.Range attrs +> dumpAndContinue
            | None ->
                rep 2 sepNln

        fun ctx ->
            match ys with
            | [] -> col sepNln xs (genSigModuleDecl astContext) ctx
            | _ -> (col sepNln xs (genSigModuleDecl astContext) +> sepXsAndYs +> genSigModuleDeclList astContext ys) ctx
"""  config
    |> prepend newline
    |> should equal """
let genSigModuleDeclList astContext node =
    match node with
    | [ x ] -> genSigModuleDecl astContext x

    | SigOpenL (xs, ys) ->
        let sepXsAndYs =
            match List.tryHead ys with
            | Some hs ->
                let attrs =
                    getRangesFromAttributesFromSynModuleSigDeclaration hs

                sepNln
                +> sepNlnConsideringTriviaContentBeforeWithAttributes hs.Range attrs
                +> dumpAndContinue
            | None -> rep 2 sepNln

        fun ctx ->
            match ys with
            | [] -> col sepNln xs (genSigModuleDecl astContext) ctx
            | _ ->
                (col sepNln xs (genSigModuleDecl astContext)
                 +> sepXsAndYs
                 +> genSigModuleDeclList astContext ys)
                    ctx
"""

[<Test>]
let ``determine lower or uppercase in DotGet, 729`` () =
    formatSourceString false """namespace Foo

open System.Linq

module Bar =
    let Baz () =
        for foo in bar().OfType<SomeType>() do
            printf "baz"

        for foo in bar().meh<SomeType>() do
            printf "baz"
"""  config
    |> prepend newline
    |> should equal """
namespace Foo

open System.Linq

module Bar =
    let Baz () =
        for foo in bar().OfType<SomeType>() do
            printf "baz"

        for foo in bar().meh<SomeType> () do
            printf "baz"
"""

[<Test>]
let ``handle hash directives before equals, 728`` () =
    formatSourceString false """let Baz (firstParam: string)
#if DEBUG
            (_         : int)
#else
            (secndParam: int)
#endif
                =
        ()

    """ config
    |> prepend newline
    |> should equal """
let Baz (firstParam: string)
#if DEBUG
        (_: int)
#else
        (secndParam: int)
#endif
        =
    ()
"""

[<Test>]
let ``handle hash directives before equals, no defines`` () =
    formatSourceStringWithDefines [] """let Baz (firstParam: string)
#if DEBUG
            (_         : int)
#else
            (secndParam: int)
#endif
                =
        ()

    """ config
    |> prepend newline
    |> should equal """
let Baz (firstParam: string)
#if DEBUG

#else
        (secndParam: int)
#endif
        =
    ()
"""

[<Test>]
let ``multiple empty lines between equals and expression`` () =
    formatSourceString false """let Baz (firstParam: string)
#if DEBUG
            (_         : int)
#else
            (secndParam: int)
#endif
                =


        ()

    """ config
    |> should equal """let Baz (firstParam: string)
#if DEBUG
        (_: int)
#else
        (secndParam: int)
#endif
        =


    ()
"""

[<Test>]
let ``don't add newline before paren tuple return value`` () =
    formatSourceString false """
/// Returns a  list of income and expense of the current month
let useEntries month year =
    let { Events = events } = useModel ()

    let isNotCancelled =
        Projections.isNotCancelledEventChecker events

    let filter = Projections.isInMonth month year

    let sortMapAndToArray (input: Transaction seq) =
        input
        |> Seq.sortBy (fun ai -> ai.Created)
        |> Seq.map (fun ai ->
            {| id = ai.Id
               name = ai.Name
               amount = ai.Amount |})
        |> Seq.toArray

    let income =
        events
        |> Seq.choose (function
            | Event.AddIncome (ai) when (filter ai.Created && isNotCancelled ai.Id) -> Some ai
            | _ -> None)
        |> sortMapAndToArray

    let expenses =
        events
        |> Seq.choose (function
            | Event.AddExpense (ae) when (filter ae.Created && isNotCancelled ae.Id) -> Some ae
            | _ -> None)
        |> sortMapAndToArray

    (income, expenses)
"""  config
    |> prepend newline
    |> should equal """
/// Returns a  list of income and expense of the current month
let useEntries month year =
    let { Events = events } = useModel ()

    let isNotCancelled =
        Projections.isNotCancelledEventChecker events

    let filter = Projections.isInMonth month year

    let sortMapAndToArray (input: Transaction seq) =
        input
        |> Seq.sortBy (fun ai -> ai.Created)
        |> Seq.map
            (fun ai ->
                {| id = ai.Id
                   name = ai.Name
                   amount = ai.Amount |})
        |> Seq.toArray

    let income =
        events
        |> Seq.choose
            (function
            | Event.AddIncome (ai) when (filter ai.Created && isNotCancelled ai.Id) -> Some ai
            | _ -> None)
        |> sortMapAndToArray

    let expenses =
        events
        |> Seq.choose
            (function
            | Event.AddExpense (ae) when (filter ae.Created && isNotCancelled ae.Id) -> Some ae
            | _ -> None)
        |> sortMapAndToArray

    (income, expenses)
"""

[<Test>]
let ``keep newline before try with`` () =
    formatSourceString false """
let private authenticateRequest (logger: ILogger) header =
    let token =
        System.Text.RegularExpressions.Regex.Replace(header, "bearer\s?", System.String.Empty)

    printfn "token: %s" token
    Microsoft.IdentityModel.Logging.IdentityModelEventSource.ShowPII <- true
    let parameters = TokenValidationParameters()
    parameters.ValidIssuer <- (sprintf "https://%s/" Auth0Domain)
    parameters.ValidAudiences <- Auth0Audiences
    parameters.ValidateIssuer <- true
    parameters.NameClaimType <- ClaimTypes.NameIdentifier // Auth0 related, see https://community.auth0.com/t/access-token-doesnt-contain-a-sub-claim/17671/2

    let manager =
        ConfigurationManager<OpenIdConnectConfiguration>
            (sprintf "https://%s/.well-known/openid-configuration" Auth0Domain, OpenIdConnectConfigurationRetriever())

    let handler = JwtSecurityTokenHandler()

    try
        task {
            let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
            parameters.IssuerSigningKeys <- config.SigningKeys

            let user, _ =
                handler.ValidateToken((token: string), parameters)

            if user.HasPermission("use:application") then
                return Some user.Identity.Name
            else
                logger.LogError(sprintf "User has a valid token but lacks the correct permission")
                return None
        }
    with exn ->
        logger.LogError(sprintf "Could not authenticate token %s\n%A" token exn)
        task { return None }
"""  config
    |> prepend newline
    |> should equal """
let private authenticateRequest (logger: ILogger) header =
    let token =
        System.Text.RegularExpressions.Regex.Replace(header, "bearer\s?", System.String.Empty)

    printfn "token: %s" token
    Microsoft.IdentityModel.Logging.IdentityModelEventSource.ShowPII <- true
    let parameters = TokenValidationParameters()
    parameters.ValidIssuer <- (sprintf "https://%s/" Auth0Domain)
    parameters.ValidAudiences <- Auth0Audiences
    parameters.ValidateIssuer <- true
    parameters.NameClaimType <- ClaimTypes.NameIdentifier // Auth0 related, see https://community.auth0.com/t/access-token-doesnt-contain-a-sub-claim/17671/2

    let manager =
        ConfigurationManager<OpenIdConnectConfiguration>(
            sprintf "https://%s/.well-known/openid-configuration" Auth0Domain,
            OpenIdConnectConfigurationRetriever()
        )

    let handler = JwtSecurityTokenHandler()

    try
        task {
            let! config =
                manager
                    .GetConfigurationAsync()
                    .ConfigureAwait(false)

            parameters.IssuerSigningKeys <- config.SigningKeys

            let user, _ =
                handler.ValidateToken((token: string), parameters)

            if user.HasPermission("use:application") then
                return Some user.Identity.Name
            else
                logger.LogError(sprintf "User has a valid token but lacks the correct permission")
                return None
        }
    with exn ->
        logger.LogError(sprintf "Could not authenticate token %s\n%A" token exn)
        task { return None }
"""

[<Test>]
let ``don't add additional newline before anonymous record`` () =
    formatSourceString false """
let useOverviewPerMonth () =
    let { Events = events } = useModel ()

    let months =
        events
        |> List.choose (fun msg ->
            match msg with
            | Event.AddIncome ({ Created = created })
            | Event.AddExpense ({ Created = created }) -> Some(created.Month, created.Year)
            | _ -> None)
        |> List.distinct
        |> List.sort
        |> List.groupBy snd
        |> List.map (fun (year, months) ->
            let rows =
                months
                |> List.map (fun (m, y) ->
                    {| name = getMonthName m
                       month = m
                       balance = Projections.calculateBalance m y events |})
                |> List.toArray

            let balance =
                rows |> Array.sumBy (fun mth -> mth.balance)

            {| name = year
               months = rows
               balance = balance |})
        |> List.toArray

    months
"""  config
    |> prepend newline
    |> should equal """
let useOverviewPerMonth () =
    let { Events = events } = useModel ()

    let months =
        events
        |> List.choose
            (fun msg ->
                match msg with
                | Event.AddIncome ({ Created = created })
                | Event.AddExpense ({ Created = created }) -> Some(created.Month, created.Year)
                | _ -> None)
        |> List.distinct
        |> List.sort
        |> List.groupBy snd
        |> List.map
            (fun (year, months) ->
                let rows =
                    months
                    |> List.map
                        (fun (m, y) ->
                            {| name = getMonthName m
                               month = m
                               balance = Projections.calculateBalance m y events |})
                    |> List.toArray

                let balance =
                    rows |> Array.sumBy (fun mth -> mth.balance)

                {| name = year
                   months = rows
                   balance = balance |})
        |> List.toArray

    months
"""

[<Test>]
let ``don't add newline before array, 1033`` () =
    formatSourceString false """
    let private additionalRefs =
        let refs =
            Directory.EnumerateFiles(Path.GetDirectoryName(typeof<System.Object>.Assembly.Location))
            |> Seq.filter (fun path -> Array.contains (Path.GetFileName(path)) assemblies)
            |> Seq.map (sprintf "-r:%s")

        [| "--simpleresolution"
           "--noframework"
           yield! refs |]
"""  config
    |> prepend newline
    |> should equal """
let private additionalRefs =
    let refs =
        Directory.EnumerateFiles(Path.GetDirectoryName(typeof<System.Object>.Assembly.Location))
        |> Seq.filter (fun path -> Array.contains (Path.GetFileName(path)) assemblies)
        |> Seq.map (sprintf "-r:%s")

    [| "--simpleresolution"
       "--noframework"
       yield! refs |]
"""

[<Test>]
let ``preserve new line new instance of class, 1034`` () =
    formatSourceString false """
    let notFound () =
        let json = Encode.string "Not found" |> Encode.toString 4

        new HttpResponseMessage(HttpStatusCode.NotFound,
                                Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json"))
"""
        { config with
              MaxValueBindingWidth = 50
              MaxFunctionBindingWidth = 50 }
    |> prepend newline
    |> should equal """
let notFound () =
    let json = Encode.string "Not found" |> Encode.toString 4

    new HttpResponseMessage(
        HttpStatusCode.NotFound,
        Content = new StringContent(json, System.Text.Encoding.UTF8, "application/json")
    )
"""

[<Test>]
let ``don't add additional newline before SynExpr.New, 1049`` () =
    formatSourceString false """
    let getVersion () =
        let version =
            let assembly =
                typeof<FSharp.Compiler.SourceCodeServices.FSharpChecker>.Assembly

            let version = assembly.GetName().Version
            sprintf "%i.%i.%i" version.Major version.Minor version.Revision

        new HttpResponseMessage(HttpStatusCode.OK,
                                Content = new StringContent(version, System.Text.Encoding.UTF8, "application/text"))
"""  config
    |> prepend newline
    |> should equal """
let getVersion () =
    let version =
        let assembly =
            typeof<FSharp.Compiler.SourceCodeServices.FSharpChecker>
                .Assembly

        let version = assembly.GetName().Version
        sprintf "%i.%i.%i" version.Major version.Minor version.Revision

    new HttpResponseMessage(
        HttpStatusCode.OK,
        Content = new StringContent(version, System.Text.Encoding.UTF8, "application/text")
    )
"""

[<Test>]
let ``sequential after local let bindings should respect indentation, 1054`` () =
    formatSourceString false "
let merge a b =
    let aChunks = splitWhenHash a
    let bChunks = splitWhenHash b

    if List.length aChunks <> List.length bChunks then
        Dbg.print (aChunks, bChunks)
        failwithf \"\"\"Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back togheter. Please raise an issue at https://github.com/fsprojects/fantomas/issues.\"\"\"

    List.zip aChunks bChunks
    |> List.map (fun (a', b') ->
        let la = lengthWithoutSpaces a'
        let lb = lengthWithoutSpaces b'
        if la <> lb then
            if la > lb then a' else b'
        else
            if String.length a' < String.length b' then a' else b'
    )

    |> String.concat Environment.NewLine
"    config
    |> prepend newline
    |> should equal "
let merge a b =
    let aChunks = splitWhenHash a
    let bChunks = splitWhenHash b

    if List.length aChunks <> List.length bChunks then
        Dbg.print (aChunks, bChunks)

        failwithf
            \"\"\"Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back togheter. Please raise an issue at https://github.com/fsprojects/fantomas/issues.\"\"\"

    List.zip aChunks bChunks
    |> List.map
        (fun (a', b') ->
            let la = lengthWithoutSpaces a'
            let lb = lengthWithoutSpaces b'

            if la <> lb then
                if la > lb then a' else b'
            else if String.length a' < String.length b' then
                a'
            else
                b')

    |> String.concat Environment.NewLine
"

[<Test>]
let ``multiline expressions within sequential should be separated with new lines`` () =
    formatSourceString false """
let x =
    if someCondition then
        //
        foo
    else
        //
        bar
    while someCondition do
        printfn "meh"
    ()
"""  config
    |> prepend newline
    |> should equal """
let x =
    if someCondition then
        //
        foo
    else
        //
        bar

    while someCondition do
        printfn "meh"

    ()
"""

[<Test>]
let ``preserve in keyword via trivia, 340`` () =
    formatSourceString false """
let x = List.singleton <|
        let item = "text" in
        item
"""  config
    |> prepend newline
    |> should equal """
let x =
    List.singleton
    <| let item = "text" in
       item
"""

[<Test>]
let ``in keyword in boolean expression, 1114`` () =
    formatSourceString false """
let x =
    not (isObjTy g ty)
    && isAppTy g ty
    && isObjTy g minfo.ApparentEnclosingType
    && let tcref = tcrefOfAppTy g ty in
       match tcref.TypeReprInfo with
       | TProvidedTypeExtensionPoint info ->
           info.ProvidedType.PUntaint(
               (fun st ->
                   (st :> IProvidedCustomAttributeProvider)
                       .GetHasTypeProviderEditorHideMethodsAttribute(info.ProvidedType.TypeProvider.PUntaintNoFailure(id))),
               m
           )
       | _ ->
           if tcref.IsILTycon then
               tcref.ILTyconRawMetadata.CustomAttrs.AsArray
               |> Array.exists (fun attr ->
                   attr.Method.DeclaringType.TypeSpec.Name = typeof<TypeProviderEditorHideMethodsAttribute>.FullName)
           else
               false
"""  config
    |> prepend newline
    |> should equal """
let x =
    not (isObjTy g ty)
    && isAppTy g ty
    && isObjTy g minfo.ApparentEnclosingType
    && let tcref = tcrefOfAppTy g ty in

       match tcref.TypeReprInfo with
       | TProvidedTypeExtensionPoint info ->
           info.ProvidedType.PUntaint(
               (fun st ->
                   (st :> IProvidedCustomAttributeProvider)
                       .GetHasTypeProviderEditorHideMethodsAttribute(info.ProvidedType.TypeProvider.PUntaintNoFailure
                                                                         (id))),
               m
           )
       | _ ->
           if tcref.IsILTycon then
               tcref.ILTyconRawMetadata.CustomAttrs.AsArray
               |> Array.exists
                   (fun attr ->
                       attr.Method.DeclaringType.TypeSpec.Name = typeof<TypeProviderEditorHideMethodsAttribute>
                           .FullName)
           else
               false
"""

[<Test>]
let ``app tuple inside dotget expression`` () =
    formatSourceString false """
                   (st :> IProvidedCustomAttributeProvider)
                       .GetHasTypeProviderEditorHideMethodsAttribute(
                           info.ProvidedType.TypeProvider.PUntaintNoFailure(
                                id
                           )
                        )

"""  { config with MaxLineLength = 40 }
    |> prepend newline
    |> should equal """
(st :> IProvidedCustomAttributeProvider)
    .GetHasTypeProviderEditorHideMethodsAttribute(info.ProvidedType.TypeProvider.PUntaintNoFailure
                                                      (id))
"""

[<Test>]
let ``in keyword in short boolean expression, 1032`` () =
    formatSourceString false """
let internal sepSpace =
    // ignore multiple spaces, space on start of file, after newline
    // TODO: this is inefficient - maybe remember last char written?
    fun (ctx: Context) ->
        if (not ctx.WriterInitModel.IsDummy && let s = dump ctx in s = "" || s.EndsWith " " || s.EndsWith Environment.NewLine) then ctx
        else (!- " ") ctx
"""  config
    |> prepend newline
    |> should equal """
let internal sepSpace =
    // ignore multiple spaces, space on start of file, after newline
    // TODO: this is inefficient - maybe remember last char written?
    fun (ctx: Context) ->
        if (not ctx.WriterInitModel.IsDummy
            && let s = dump ctx in

               s = ""
               || s.EndsWith " "
               || s.EndsWith Environment.NewLine) then
            ctx
        else
            (!- " ") ctx
"""

[<Test>]
let ``in keyword in LetOrUse with and keyword, 1176`` () =
    formatSourceString false """
do
    let rec f = ()
    and g = () in
    ()
"""  config
    |> prepend newline
    |> should equal """
do
    let rec f = ()
    and g = () in
    ()
"""

[<Test>]
let nameof () =
    formatSourceString false """
let months =
    [
        "January"; "February"; "March"; "April";
        "May"; "June"; "July"; "August"; "September";
        "October"; "November"; "December"
    ]

let lookupMonth month =
    if (month > 12 || month < 1) then
        invalidArg (nameof month) (sprintf "Value passed in was %d." month)

    months.[month-1]

printfn "%s" (lookupMonth 12)
printfn "%s" (lookupMonth 1)
printfn "%s" (lookupMonth 13) // Throws an exception!
"""  config
    |> prepend newline
    |> should equal """
let months =
    [ "January"
      "February"
      "March"
      "April"
      "May"
      "June"
      "July"
      "August"
      "September"
      "October"
      "November"
      "December" ]

let lookupMonth month =
    if (month > 12 || month < 1)
    then invalidArg (nameof month) (sprintf "Value passed in was %d." month)

    months.[month - 1]

printfn "%s" (lookupMonth 12)
printfn "%s" (lookupMonth 1)
printfn "%s" (lookupMonth 13) // Throws an exception!
"""

[<Test>]
let ``print inline before private, 1250`` () =
    formatSourceString false """
    let inline private isIdentifier t = t.CharClass = FSharpTokenCharKind.Identifier
    let inline private isOperator t = t.CharClass = FSharpTokenCharKind.Operator
    let inline private isKeyword t = t.ColorClass = FSharpTokenColorKind.Keyword
    let inline private isPunctuation t = t.ColorClass = FSharpTokenColorKind.Punctuation
"""  config
    |> prepend newline
    |> should equal """
let inline private isIdentifier t =
    t.CharClass = FSharpTokenCharKind.Identifier

let inline private isOperator t =
    t.CharClass = FSharpTokenCharKind.Operator

let inline private isKeyword t =
    t.ColorClass = FSharpTokenColorKind.Keyword

let inline private isPunctuation t =
    t.ColorClass = FSharpTokenColorKind.Punctuation
"""

[<Test>]
let ``comment after equal sign of value binding, 1248`` () =
    formatSourceString false """
let value = // TODO: some comment
    let v = 2 + 3
    v

let k = -1
"""  config
    |> prepend newline
    |> should equal """
let value = // TODO: some comment
    let v = 2 + 3
    v

let k = -1
"""

[<Test>]
let ``comment after equal sign of function binding`` () =
    formatSourceString false """
let value a = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""  config
    |> prepend newline
    |> should equal """
let value a = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""

[<Test>]
let ``comment after equal sign of function binding, AlignFunctionSignatureToIndentation`` () =
    formatSourceString false """
let longFunctionNameThatWillTriggerAlternativeSignatureSyntax a = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""
        { config with
              AlignFunctionSignatureToIndentation = true
              MaxLineLength = 60 }
    |> prepend newline
    |> should equal """
let longFunctionNameThatWillTriggerAlternativeSignatureSyntax
    a
    = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""

[<Test>]
let ``comment after equal sign of function binding with return type`` () =
    formatSourceString false """
let value a : int = // TODO: some comment
    let v x : int = 2 + a
    v

let k = -1
"""  config
    |> prepend newline
    |> should equal """
let value a: int = // TODO: some comment
    let v x: int = 2 + a
    v

let k = -1
"""

[<Test>]
let ``comment after equal sign of function binding with return type, AlignFunctionSignatureToIndentation`` () =
    formatSourceString false """
let longFunctionNameThatWillTriggerAlternativeSignatureSyntax a : int = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""
        { config with
              AlignFunctionSignatureToIndentation = true
              MaxLineLength = 60 }
    |> prepend newline
    |> should equal """
let longFunctionNameThatWillTriggerAlternativeSignatureSyntax
    a
    : int
    = // TODO: some comment
    let v = 2 + a
    v

let k = -1
"""
