module Fantomas.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the |> is merged to the last line
[<Test>]
let ``no nln before lambda #503`` () =
    formatSourceString false """
let a =
    b
    |> List.exists (fun p ->
        p.a && p.b |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
    """ { config with MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
let a =
    b
    |> List.exists
        (fun p ->
            p.a
            && p.b
               |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
"""

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545`` () =
    formatSourceString false @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               s
                                     s" config
    |> prepend newline
    |> should equal @"
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
    formatSourceString false @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""                (llloooooooooooooooooooooooooo s)
                            s
                               (llloooooooooooooooooooooooooo s)
                                     (llloooooooooooooooooooooooooo s)" { config with MaxLineLength = 50 }
    |> prepend newline
    |> should equal @"
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
    formatSourceString false """module Caching =
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
            ()""" { config with MaxLineLength = 60 }
    |> prepend newline
    |> should equal """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency)
                                              : NotFresh<decimal> =
            lock
                cacheFiles.CachedNetworkData
                (fun _ ->
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
    formatSourceString false """module Caching =
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
            ()""" { config with MaxLineLength = 60 }
    |> prepend newline
    |> should equal """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress)
                                              (currency: Currency)
                                              : NotFresh<decimal> =
            lock
                cacheFiles.CachedNetworkData
                (fun _ ->
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
    formatSourceString false """module Caching =
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
            ()""" { config with MaxLineLength = 120 }
    |> prepend newline
    |> should equal """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency): NotFresh<decimal> =
            lock
                cacheFiles.CachedNetworkData
                (fun _ ->
                    match balance with
                    | NotAvailable -> NotAvailable
                    | Cached (balance, time) ->
                        if compoundBalance < 0.0m
                        then ReportProblem compoundBalance None currency address sessionCachedNetworkData

                        ())

            ()
"""

[<Test>]
let ``should not split single parameter over multiple lines when it does not exceed page width`` () =
    formatSourceString false """module Caching =
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
            ()""" { config with MaxLineLength = 120 }
    |> prepend newline
    |> should equal """
module Caching =
    type MainCache() =
        member __.RetrieveLastCompoundBalance (address: PublicAddress) (currency: Currency): NotFresh<decimal> =
            lock
                cacheFiles.CachedNetworkData
                (fun _ ->
                    match balance with
                    | NotAvailable -> NotAvailable
                    | Cached (balance, time) ->
                        if compoundBalance < 0.0m then ReportProblem compoundBalance
                        ())

            ()
"""

[<Test>]
let ``single line constructor without new keyword`` () =
    formatSourceString false """
let smallTree = BinaryNode(BinaryValue 3, BinaryValue 4)
"""  config
    |> prepend newline
    |> should equal """
let smallTree = BinaryNode(BinaryValue 3, BinaryValue 4)
"""

[<Test>]
let ``multiline constructor without new keyword`` () =
    formatSourceString false """
let tree1 =
    BinaryNode(BinaryNode(BinaryValue 1, BinaryValue 2), BinaryNode(BinaryValue 3, BinaryValue 4))

"""  { config with MaxLineLength = 80 }
    |> prepend newline
    |> should equal """
let tree1 =
    BinaryNode(
        BinaryNode(BinaryValue 1, BinaryValue 2),
        BinaryNode(BinaryValue 3, BinaryValue 4)
    )
"""

[<Test>]
let ``short constructor with new keyword`` () =
    formatSourceString false """
let person = new Person("Jim", 33)
"""  config
    |> prepend newline
    |> should equal """
let person = new Person("Jim", 33)
"""

[<Test>]
let ``multiline constructor with new keyword`` () =
    formatSourceString false """
let otherThing =
    new Foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""  { config with MaxLineLength = 90 }
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
let myRegexMatch = Regex.Match(input, regex)
"""  config
    |> prepend newline
    |> should equal """
let myRegexMatch = Regex.Match(input, regex)
"""

[<Test>]
let ``multiline static member call`` () =
    formatSourceString false """
let myRegexMatch =
    Regex.Match("my longer input string with some interesting content in it","myRegexPattern")
"""  { config with MaxLineLength = 90 }
    |> prepend newline
    |> should equal """
let myRegexMatch =
    Regex.Match(
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )
"""

[<Test>]
let ``short instance member call`` () =
    formatSourceString false """
let untypedRes = checker.ParseFile(file, source, opts)
"""  config
    |> prepend newline
    |> should equal """
let untypedRes = checker.ParseFile(file, source, opts)
"""

[<Test>]
let ``multiline instance member call`` () =
    formatSourceString false """
let untypedRes =
    checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""  { config with MaxLineLength = 90 }
    |> prepend newline
    |> should equal """
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
    formatSourceString false "
[<Test>]
let ``classes and private implicit constructors`` () =
    formatSourceString false \"\"\"
    type MyClass2 private (dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf \"Creating MyClass2 with Data %d\" data\"\"\" { config with
                                                                 MaxFunctionBindingWidth = 120 }
"    config
    |> prepend newline
    |> should equal "
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
        { config with
              MaxFunctionBindingWidth = 120 }
"

[<Test>]
let ``space between function and argument in DotIndexedGet, 1261`` () =
    formatSourceString false """
type Queue<'T>(data: list<'T []>, length: int) =

    member this.Head =
        if length > 0
        then (List.head data).[0]
        else raise (System.Exception("Queue is empty"))
"""  config
    |> prepend newline
    |> should equal """
type Queue<'T>(data: list<'T []>, length: int) =

    member this.Head =
        if length > 0 then
            (List.head data).[0]
        else
            raise (System.Exception("Queue is empty"))
"""
