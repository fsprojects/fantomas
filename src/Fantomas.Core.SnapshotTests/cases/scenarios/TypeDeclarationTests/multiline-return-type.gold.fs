type Meh =
    static member AsBeginEnd<'Arg, 'T>
        (computation: ('Arg -> Async<'T>))
        // The 'Begin' member
        : ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
          (System.IAsyncResult -> 'T) *
          (System.IAsyncResult -> unit)
        =
        let beginAction =
            fun (a1, callback, state) -> AsBeginEndHelpers.beginAction ((computation a1), callback, state)

        beginAction, AsBeginEndHelpers.endAction<'T>, AsBeginEndHelpers.cancelAction<'T>
