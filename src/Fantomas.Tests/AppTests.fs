module Fantomas.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the |> is merged to the last line
[<Test>]
let ``no nln before lambda, #503`` () =
    formatSourceString
        false
        """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    """
        { config with MaxLineLength = 80 }
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
        false
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
        false
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
        false
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
                | Cached (balance, time) ->
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
        false
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
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem
                            looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong

                    ())

            ()
"""

[<Test>]
let ``should not split parameters over multiple lines when they do not exceed page width`` () =
    formatSourceString
        false
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
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance None currency address sessionCachedNetworkData

                    ())

            ()
"""

[<Test>]
let ``should not split single parameter over multiple lines when it does not exceed page width`` () =
    formatSourceString
        false
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
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then
                        ReportProblem compoundBalance

                    ())

            ()
"""

[<Test>]
let ``single line constructor without new keyword`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        config
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
        false
        """
type Queue<'T>(data: list<'T []>, length: int) =

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
type Queue<'T>(data: list<'T []>, length: int) =

    member this.Head =
        if length > 0 then
            (List.head data).[0]
        else
            raise (System.Exception("Queue is empty"))
"""

[<Test>]
let ``parenthesis around composed function expression, 1341`` () =
    formatSourceString
        false
        """
(f >> g) (bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb, c)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(f >> g)
    (
        bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,
        c
    )
"""

[<Test>]
let ``parenthesis around short composed function expression, tuple, 1700`` () =
    formatSourceString false """((=) (ownerName, username))""" config
    |> should
        equal
        """((=) (ownerName, username))
"""

[<Test>]
let ``parenthesis around short composed function expression, tuple in if, 1700`` () =
    formatSourceString false """if ((=) (ownerName, username)) then 6""" config
    |> should
        equal
        """if ((=) (ownerName, username)) then 6
"""

[<Test>]
let ``parenthesis around short composed function expression, no tuple, 1700`` () =
    formatSourceString false """((=) ownerName)""" config
    |> should
        equal
        """((=) ownerName)
"""

[<Test>]
let ``parenthesis around short composed function expression, no tuple in if, 1700, part 2`` () =
    formatSourceString false """if ((=) ownerName) then 6""" config
    |> should
        equal
        """if ((=) ownerName) then 6
"""


[<Test>]
let ``parenthesis around simple function expression`` () =
    formatSourceString
        false
        """
(ignore) ("Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone", 42)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(ignore)
    (
        "Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone",
        42
    )
"""

[<Test>]
let ``parenthesis around simple function expression with single parenthesis argument`` () =
    formatSourceString
        false
        """
(ignore) ("Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone")
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
(ignore)
    ("Tuuuuuuuuuuuuurn tooooooooooooooooooooooo stooooooooooooooooooooooooone")
"""

[<Test>]
let ``parenthesis around simple function expression with single multiline parenthesis argument`` () =
    formatSourceString
        false
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
(ignore)
    (
        \"\"\"Tuuuuuuuuuuuuurn
tooooooooooooooooooooooo
stooooooooooooooooooooooooone\"\"\"
    )
"

[<Test>]
let ``multiline function application with single unit argument, 1469`` () =
    formatSourceString
        false
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
            initialFeeWithAMinimumGasPriceInWeiDictatedByAvailablePublicFullNodes.CalculateAbsoluteValue
                ()

        initialAbs / 100
"""

[<Test>]
let ``comment after app with single tuple arg, 1276`` () =
    formatSourceString
        false
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
        false
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
        false
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
        { config with
            IndentSize = 2
            DisableElmishSyntax = true }
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
        false
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
                | Ok (TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            ))
"""

[<Test>]
let ``comment after typeApp greater sign, 1861`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
let ``function invocation with multiple curried parameters, 2087`` () =
    formatSourceString
        false
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
