[<Sealed>]
type MaybeBuilder() =
    // M<'T> * ('T -> M<'U>) -> M<'U>
#if DEBUG
    member __.Bind
#else
    member inline __.Bind
#endif
        (value, binder: 'T -> 'U option) : 'U option =
        Option.bind binder value
