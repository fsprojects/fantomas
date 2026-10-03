module Caching =
    type MainCache() =
        member __.GetLastCachedData() : CachedNetworkData =
            lock
                cacheFiles.CachedNetworkData
                (fun _ -> sessionCachedNetworkData)
