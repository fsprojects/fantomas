[<RequireQualifiedAccess>]
module Example =

    let dict1 = ConcurrentDictionary< (* some comment 1 *) int32, unit>()

    let dict2 =
        ConcurrentDictionary< (* some comment 2 *) int64, ConcurrentDictionary< (* some comment 3 *) int32, unit>>()
