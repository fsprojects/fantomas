module Fantomas.Core.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// the current behavior results in a compile error since the |> is merged to the last line
[<Test>]
let ``no nln before lambda, #503`` () =
    formatSourceString
        """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    """
        { config with
            MaxLineLength = 80
            MaxInfixOperatorExpression = 40 }
    |> prepend newline
    |> should
        equal
        """
let a =
    b
    |> List.exists (fun p ->
        p.a
        && p.b
           |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
"""

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545`` () =
    formatSourceString
        @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               s
                                     s"
        config
    |> prepend newline
    |> should
        equal
        @"
let a s =
    if s <> """" then
        printfn
            """"""fooo
%s
%s
%s
%s""""""
            (llloooooooooooooooooooooooooo s)
            s
            s
            s
"

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545 (long expression and short line settings)`` () =
    formatSourceString
        @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               (llloooooooooooooooooooooooooo s)
                                     (llloooooooooooooooooooooooooo s)"
        { config with MaxLineLength = 50 }
    |> prepend newline
    |> should
        equal
        @"
let a s =
    if s <> """" then
        printfn
            """"""fooo
%s
%s
%s
%s""""""
            (llloooooooooooooooooooooooooo s)
            s
            (llloooooooooooooooooooooooooo s)
            (llloooooooooooooooooooooooooo s)
"

[<Test>]
let ``should split parameters over multiple lines when they exceed page width`` () =
    formatSourceString
        """module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency): NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance None currency address sessionCachedNetworkData
                    ()
            )
            ()"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance
            (address: PublicAddress)
            (currency: Currency)
            : NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached(balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem
                            compoundBalance
                            None
                            currency
                            address
                            sessionCachedNetworkData

                    ())

            ()
"""

[<Test>]
let ``should split single parameter over multiple lines when it exceeds page width`` () =
    formatSourceString
        """module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency): NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong
                    ()
            )
            ()"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance
            (address: PublicAddress)
            (currency: Currency)
            : NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached(balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem
                            looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

                    ())

            ()
"""

[<Test>]
let ``should not split parameters over multiple lines when they do not exceed page width`` () =
    formatSourceString
        """module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency): NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance None currency address sessionCachedNetworkData
                    ()
            )
            ()"""
        { config with MaxLineLength = 120 }
    |> prepend newline
    |> should
        equal
        """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency) : NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached(balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance None currency address sessionCachedNetworkData

                    ())

            ()
"""

[<Test>]
let ``should not split single parameter over multiple lines when it does not exceed page width`` () =
    formatSourceString
        """module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency): NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance
                    ()
            )
            ()"""
        { config with MaxLineLength = 120 }
    |> prepend newline
    |> should
        equal
        """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency) : NotFresh<decimal> =
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached(balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance

                    ())

            ()
"""

[<Test>]
let ``single line constructor without new keyword`` () =
    formatSourceString
        """
let smallTree = BinaryNode(BinaryValue 3, BinaryValue 4)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let smallTree = BinaryNode(BinaryValue 3, BinaryValue 4)
"""

[<Test>]
let ``multiline constructor without new keyword`` () =
    formatSourceString
        """
let tree1 =
    BinaryNode(BinaryNode(BinaryValue 1, BinaryValue 2), BinaryNode(BinaryValue 3, BinaryValue 4))

"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    BinaryNode(
        BinaryNode(BinaryValue 1, BinaryValue 2),
        BinaryNode(BinaryValue 3, BinaryValue 4)
    )
"""

[<Test>]
let ``short constructor with new keyword`` () =
    formatSourceString
        """
let person = new Person("Jim", 33)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let person = new Person("Jim", 33)
"""

[<Test>]
let ``multiline constructor with new keyword`` () =
    formatSourceString
        """
let otherThing =
    new Foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let otherThing =
    new Foobar(
        longname1,
        longname2,
        longname3,
        longname4,
        longname5,
        longname6,
        longname7
    )
"""

[<Test>]
let ``short static member call`` () =
    formatSourceString
        """
let myRegexMatch = Regex.Match(input, regex)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch = Regex.Match(input, regex)
"""

[<Test>]
let ``multiline static member call`` () =
    formatSourceString
        """
let myRegexMatch =
    Regex.Match("my longer input string with some interesting content in it","myRegexPattern")
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch =
    Regex.Match(
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )
"""

[<Test>]
let ``short instance member call`` () =
    formatSourceString
        """
let untypedRes = checker.ParseFile(file, source, opts)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let untypedRes = checker.ParseFile(file, source, opts)
"""

[<Test>]
let ``multiline instance member call`` () =
    formatSourceString
        """
let untypedRes =
    checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let untypedRes =
    checker.ParseFile(
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``multiline string in function application, 1259`` () =
    formatSourceString
        "
[<Test>]
let ``classes and private implicit constructors`` () =
    formatSourceString false \"\"\"
    type MyClass2 private (dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf \"Creating MyClass2 with Data %d\" data\"\"\" { config with
                                                                 MaxFunctionBindingWidth = 120 }
"
        { config with MaxRecordWidth = 50 }
    |> prepend newline
    |> should
        equal
        "
[<Test>]
let ``classes and private implicit constructors`` () =
    formatSourceString
        false
        \"\"\"
    type MyClass2 private (dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf \"Creating MyClass2 with Data %d\" data\"\"\"
        { config with MaxFunctionBindingWidth = 120 }
"

[<Test>]
let ``space between function and argument in DotIndexedGet, 1261`` () =
    formatSourceString
        """
type Queue<'T>(data: list<'T[]>, length: int) =

    member this.Head =
        if length > 0
        then (List.head data).[0]
        else raise (System.Exception("Queue is empty"))
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Queue<'T>(data: list<'T[]>, length: int) =

    member this.Head =
        if length > 0 then
            (List.head data).[0]
        else
            raise (System.Exception("Queue is empty"))
"""

[<Test>]
let ``parenthesis around composed function expression, 1341`` () =
    formatSourceString
        """
(f >> g) (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, c)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(f >> g) (
    bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,
    c
)
"""

[<Test>]
let ``parenthesis around short composed function expression, tuple, 1700`` () =
    formatSourceString
        "((=) (ownerName, username))"
        { config with
            InsertFinalNewline = false }
    |> should equal "((=) (ownerName, username))"

[<Test>]
let ``parenthesis around short composed function expression, tuple in if, 1700`` () =
    formatSourceString
        "if ((=) (ownerName, username)) then 6"
        { config with
            MaxIfThenShortWidth = 40
            InsertFinalNewline = false }
    |> should equal "if ((=) (ownerName, username)) then 6"

[<Test>]
let ``parenthesis around short composed function expression, no tuple, 1700`` () =
    formatSourceString
        """((=) ownerName)"""
        { config with
            InsertFinalNewline = false }
    |> should equal """((=) ownerName)"""

[<Test>]
let ``parenthesis around short composed function expression, no tuple in if, 1700, part 2`` () =
    formatSourceString
        "if ((=) ownerName) then 6"
        { config with
            MaxIfThenShortWidth = 25
            InsertFinalNewline = false }
    |> should equal "if ((=) ownerName) then 6"

[<Test>]
let ``parenthesis around simple function expression`` () =
    formatSourceString
        """
(ignore) ("Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone", 42)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(ignore) (
    "Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone",
    42
)
"""

[<Test>]
let ``parenthesis around simple function expression with single parenthesis argument`` () =
    formatSourceString
        """
(ignore) ("Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone")
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
(ignore) (
    "Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone"
)
"""

[<Test>]
let ``parenthesis around simple function expression with single multiline parenthesis argument`` () =
    formatSourceString
        "
(ignore) (\"\"\"Tuuuuuuuuuuuuurn
tooooooooooooooooooooooo
stooooooooooooooooooooooooone\"\"\")
"
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        "
(ignore) (
    \"\"\"Tuuuuuuuuuuuuurn
tooooooooooooooooooooooo
stooooooooooooooooooooooooone\"\"\"
)
"

[<Test>]
let ``multiline function application with single unit argument, 1469`` () =
    formatSourceString
        """
namespace SomeNs

module SomeModule =
    let GetNormalAccountsPairingInfoForWatchWallet(): Option<WatchWalletInfo> =
        let initialAbs = initialFeeWithAMinimumGasPriceInWeiDictatedByAvailablePublicFullNodes.CalculateAbsoluteValue()
        initialAbs / 100
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace SomeNs

module SomeModule =
    let GetNormalAccountsPairingInfoForWatchWallet
        ()
        : Option<WatchWalletInfo> =
        let initialAbs =
            initialFeeWithAMinimumGasPriceInWeiDictatedByAvailablePublicFullNodes
                .CalculateAbsoluteValue()

        initialAbs / 100
"""

[<Test>]
let ``comment after app with single tuple arg, 1276`` () =
    formatSourceString
        """
SomeFunction(arg1,
    arg2,
    arg3) // does something
SomeOtherFunction(arg1, arg2) // does another thing
"""
        config
    |> prepend newline
    |> should
        equal
        """
SomeFunction(arg1, arg2, arg3) // does something
SomeOtherFunction(arg1, arg2) // does another thing
"""

[<Test>]
let ``comment after app with single tuple arg, multiline format`` () =
    formatSourceString
        """
SomeFunction(arg1,
    arg2,
    arg3) // does something
SomeOtherFunction(arg1, arg2) // does another thing
"""
        { config with MaxLineLength = 20 }
    |> prepend newline
    |> should
        equal
        """
SomeFunction(
    arg1,
    arg2,
    arg3
) // does something

SomeOtherFunction(
    arg1,
    arg2
) // does another thing
"""

[<Test>]
let ``string interpolation should not affect multiline function applications, 1771`` () =
    formatSourceString
        """
   let tryDataOperation  =

           let body =
             let clauses =
               [ mkSynMatchClause
                   (mkSynPatLongIdentSimple "Some")
                   (mkSynExprAppNonAtomic
                     (mkSynExprLongIdent $"this.{memberName}")
                     (mkSynExprParen (
                       mkSynExprTuple
                         [ mkSynExprIdent "state" ]
                     ))) ]

             mkSynExprMatch clauses

           mkMember $"this.Try{memberName}" None [ mkSynAttribute "CustomOperation" (mkSynExprConstString $"try{memberName}") ] [ parameters ] (objectStateExpr body)
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let tryDataOperation =

  let body =
    let clauses =
      [ mkSynMatchClause
          (mkSynPatLongIdentSimple "Some")
          (mkSynExprAppNonAtomic
            (mkSynExprLongIdent $"this.{memberName}")
            (mkSynExprParen (mkSynExprTuple [ mkSynExprIdent "state" ]))) ]

    mkSynExprMatch clauses

  mkMember
    $"this.Try{memberName}"
    None
    [ mkSynAttribute "CustomOperation" (mkSynExprConstString $"try{memberName}") ]
    [ parameters ]
    (objectStateExpr body)
"""

[<Test>]
let ``comment after lambda inside parenthesis argument, 1822`` () =
    formatSourceString
        """
module Foo =

    let blah =
        it
        |> List.iter (fun (_, output) ->
            thing
            |> Map.iter (fun key value ->
                match value with
                | Ok (TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            )
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let blah =
        it
        |> List.iter (fun (_, output) ->
            thing
            |> Map.iter (fun key value ->
                match value with
                | Ok(TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            ))
"""

[<Test>]
let ``comment after typeApp greater sign, 1861`` () =
    formatSourceString
        """
x |> f<y> // some comment
"""
        config
    |> prepend newline
    |> should
        equal
        """
x |> f<y> // some comment
"""

[<Test>]
let ``single named arguments should have space surrounding the '=', 1877`` () =
    formatSourceString
        """
let makeStreamReader x = new System.IO.StreamReader(path=x)"""
        config
    |> prepend newline
    |> should
        equal
        """
let makeStreamReader x = new System.IO.StreamReader(path = x)
"""

[<Test>]
let ``multiple named arguments should have space surrounding the '=', 1877`` () =
    formatSourceString
        """
let makeStreamReader x y = new StreamReader(arg1=x, arg2=y)"""
        config
    |> prepend newline
    |> should
        equal
        """
let makeStreamReader x y = new StreamReader(arg1 = x, arg2 = y)
"""

[<Test>]
let ``print comments before named argument application`` () =
    formatSourceString
        """
let Ok (content: string) =
// #if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
  //  #if API_GATEWAY
        //Headers = Map.empty.Add("Content-Type", "text/plain")
  //  #else
        Headers = Map.empty.Add("Content-Type", "application/json")
// #endif
    )
// #else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
// #endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Ok (content: string) =
    // #if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        //  #if API_GATEWAY
        //Headers = Map.empty.Add("Content-Type", "text/plain")
        //  #else
        Headers = Map.empty.Add("Content-Type", "application/json")
    // #endif
    )
    // #else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
// #endif
"""

[<Test>]
let ``print trivia before named argument application, 2068`` () =
    formatSourceString
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""

[<Test>]
let ``should not add newline before "let!", 1932`` () =
    formatSourceString
        """
promise {
    setItems [||]
    setFetchingItems true
    let! items = Api.fetchItems partNumber
    setFetchingItems false
}
|> Promise.start
"""
        config
    |> prepend newline
    |> should
        equal
        """
promise {
    setItems [||]
    setFetchingItems true
    let! items = Api.fetchItems partNumber
    setFetchingItems false
}
|> Promise.start
"""

[<Test>]
let ``print trivia before named argument application, API_GATEWAY`` () =
    formatSourceStringWithDefines
        [ "API_GATEWAY" ]
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
#endif
    )
#else
#endif
"""

[<Test>]
let ``print trivia before named argument application, MADAPI`` () =
    formatSourceStringWithDefines
        [ "MADAPI" ]
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
#endif
"""

[<Test>]
let ``print trivia before named argument application, no defines`` () =
    formatSourceStringWithDefines
        []
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
    APIGatewayHttpApiV2ProxyResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
#if API_GATEWAY
        Headers = Map.empty.Add("Content-Type", "text/plain")
#else
        Headers = Map.empty.Add("Content-Type", "application/json")
#endif
    )
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
let Ok (content: string) =
#if API_GATEWAY || MADAPI
#if API_GATEWAY
#else
#endif
#else
    ApplicationLoadBalancerResponse(
        StatusCode = int HttpStatusCode.OK,
        Body = content,
        Headers = Map.empty.Add("Content-Type", "text/plain")
    )
#endif
"""

[<Test>]
let ``function invocation with multiple curried parameters, 2087`` () =
    formatSourceString
        """
module Foo =
    let Bar (baz1: int) (baz2: string) (baz3: string) (baz4: string) (baz5: string) =
        FooBarBaz(someFunc x) (someOtherFunc y)
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let Bar (baz1: int) (baz2: string) (baz3: string) (baz4: string) (baz5: string) =
        FooBarBaz (someFunc x) (someOtherFunc y)
"""

[<Test>]
let ``block comment between arguments`` () =
    formatSourceString
        """
printf "%-40s %s" "" (*<--flags*) word
"""
        config
    |> prepend newline
    |> should
        equal
        """
printf "%-40s %s" "" (*<--flags*) word
"""

[<Test>]
let ``broken comma in match expression in parameter invocation, 1869`` () =
    formatSourceString
        """
module Utils

type U = A of int | B of string

type C() =
    member this.M (a : int, b : string) = ()

let f () =
    let u = A 0
    do C().M(
        match u with
             | A i -> i
             | B _ -> 0
             ,
        match u with
             | A _ -> ""
             | B s -> s
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Utils

type U =
    | A of int
    | B of string

type C() =
    member this.M(a: int, b: string) = ()

let f () =
    let u = A 0

    do
        C()
            .M(
                match u with
                | A i -> i
                | B _ -> 0
                , match u with
                  | A _ -> ""
                  | B s -> s
            )
"""

[<Test>]
let ``function invocation wrapped in parentheses, 2382`` () =
    formatSourceString
        """
(ignore)
    (
        "Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone",
        42
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
(ignore) (
    "Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone",
    42
)
"""

[<Test>]
let ``extra indent in multiline application`` () =
    formatSourceString
        """
((aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa b c))
"""
        { config with
            IndentSize = 2
            MaxLineLength = 0 }
    |> prepend newline
    |> should
        equal
        """
((aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    b
    c))
"""

[<Test>]
let ``multiline application wrapped in parentheses that equal the indent_size, 2943`` () =
    formatSourceString
        """
((Combo { e1 = "Making this long so it goes on a new line new line new line new line making it long so it goes on a new line new line" }))
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
((Combo
    { e1 =
        "Making this long so it goes on a new line new line new line new line making it long so it goes on a new line new line" }))
"""

[<Test>]
let ``atCurrentColumn multiline application does not need addition indent`` () =
    formatSourceString
        """
foo {
    bar in ((((aaaaaaaaaaaa b c))))
}
"""
        { config with MaxLineLength = 0 }
    |> prepend newline
    |> should
        equal
        """
foo {
    bar in ((((aaaaaaaaaaaa
                   b
                   c))))
}
"""

[<Test>]
let ``don't indent function application arguments when function name is further indented`` () =
    formatSourceString
        """
((((((((((((((((((((((
    f a b c
))))))))))))))))))))))
"""
        { config with MaxLineLength = 0 }
    |> prepend newline
    |> should
        equal
        """
((((((((((((((((((((((f
    a
    b
    c))))))))))))))))))))))
"""

[<Test>]
let ``type annotation spacing should not be adjusted for previous expression length, 3179`` () =
    formatSourceString
        """
let x =
            (someFunctionCall()) |> anotherOne |> anotherOne |> alsoAnotherOne

            Unchecked.defaultof<_>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne

    Unchecked.defaultof<_>
"""

[<Test>]
let ``type annotation spacing should not be adjusted for previous expression length without blank line, 3179`` () =
    formatSourceString
        """
let x =
            (someFunctionCall()) |> anotherOne |> anotherOne |> alsoAnotherOne
            Unchecked.defaultof<_>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne
    Unchecked.defaultof<_>
"""

[<Test>]
let ``type annotation spacing should not be adjusted for short previous expression, 3179`` () =
    formatSourceString
        """
let x =
            someFunction()

            Unchecked.defaultof<_>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    someFunction ()

    Unchecked.defaultof<_>
"""

[<Test>]
let ``type annotation spacing should not be adjusted for different type applications, 3179`` () =
    formatSourceString
        """
let x =
            (someFunctionCall()) |> anotherOne |> anotherOne |> alsoAnotherOne

            List.map<int, string>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne

    List.map<int, string>
"""

[<Test>]
let ``type annotation spacing should not be adjusted for generic type applications, 3179`` () =
    formatSourceString
        """
let x =
            (someFunctionCall()) |> anotherOne |> anotherOne |> alsoAnotherOne

            Some<'T>.Create
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne

    Some<'T>.Create
"""

[<Test>]
let ``function application type parameters should still be aligned correctly, 3179`` () =
    formatSourceString
        """
let x = someFunction<int, string> (arg1, arg2)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = someFunction<int, string> (arg1, arg2)
"""
