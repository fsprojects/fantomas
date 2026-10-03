[<Sealed>]
type MaybeBuilder() =
    // M<'T> * ('T -> M<'U>) -> M<'U>
    #if DEBUG
    member __.Bind
        #else
        #endif
        (value, binder: 'T -> 'U option) : 'U option =
        Option.bind binder value
