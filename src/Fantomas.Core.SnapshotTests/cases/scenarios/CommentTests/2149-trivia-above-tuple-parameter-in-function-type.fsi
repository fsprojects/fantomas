namespace Foo

type X =
    static member AsBeginEnd : computation:('Arg -> Async<'T>) ->
                                    // The 'Begin' member
                                    ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
                                    // The 'End' member
                                    (System.IAsyncResult -> 'T) *
                                    // The 'Cancel' member
                                    (System.IAsyncResult -> unit)
