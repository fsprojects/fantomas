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
