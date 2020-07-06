module Fantomas.Tests.AppTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// the current behavior results in a compile error since the |> is merged to the last line 
[<Test>]
let ``no nln before lambda #503``() =
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
    |> List.exists (fun p ->
        p.a
        && p.b
        |> List.exists (fun o -> o.a = "lorem ipsum dolor sit amet"))
"""

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545``() =
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
                                     s"
        config
    |> prepend newline
    |> should equal @"
let a s =
    if s <> """"
    then printfn """"""fooo
%s
%s
%s
%s""""""     (llloooooooooooooooooooooooooo s) s s s
"

// compile error due to expression starting before the beginning of the function expression
[<Test>]
let ``require to ident at least +1 after function name #545 (long expression and short line settings)``() =
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
                                     (llloooooooooooooooooooooooooo s)"
        { config  with MaxLineLength = 50 } 
    |> prepend newline
    |> should equal @"
let a s =
    if s <> """" then
        printfn """"""fooo
%s
%s
%s
%s""""""    (llloooooooooooooooooooooooooo s) s
            (llloooooooooooooooooooooooooo s)
            (llloooooooooooooooooooooooooo s)
"

[<Test>]
let ``should split parameters over multiple lines when they exceed page width``() =
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
let ``should split single parameter over multiple lines when it exceeds page width``() =
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
let ``should not split parameters over multiple lines when they do not exceed page width``() =
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
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m
                    then ReportProblem compoundBalance None currency address sessionCachedNetworkData
                    ())
            ()
"""

[<Test>]
let ``should not split single parameter over multiple lines when it does not exceed page width``() =
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
            lock cacheFiles.CachedNetworkData (fun _ ->
                match balance with
                | NotAvailable -> NotAvailable
                | Cached (balance, time) ->
                    if compoundBalance < 0.0m then ReportProblem compoundBalance
                    ())
            ()
"""
