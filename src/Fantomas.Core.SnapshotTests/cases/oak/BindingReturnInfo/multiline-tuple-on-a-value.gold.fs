let actions
    : ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
      (System.IAsyncResult -> 'T) *
      (System.IAsyncResult -> unit) =
    beginAction, endAction, cancelAction
