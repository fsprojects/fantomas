type MaybeBuilder () =
    member inline __.Bind
// meh
        (value, binder : 'T -> 'U option) : 'U option =
        Option.bind binder value
