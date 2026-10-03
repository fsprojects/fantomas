type Chapter() =
    member val Title: string option = Unchecked.defaultof<_> with get, set
    member val Url: string = Unchecked.defaultof<_> with get, set

let c =
    Chapter(
        if true then Some "" else None
        , ""
    )
