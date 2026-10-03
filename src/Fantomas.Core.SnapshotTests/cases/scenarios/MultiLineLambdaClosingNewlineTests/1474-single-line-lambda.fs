(*---
max_line_length = 80
fsharp_multi_line_lambda_closing_newline = true
---*)
module Caching =
    type MainCache() =
        member __.GetLastCachedData (): CachedNetworkData =
            lock cacheFiles.CachedNetworkData (fun _ ->
                sessionCachedNetworkData
            )
