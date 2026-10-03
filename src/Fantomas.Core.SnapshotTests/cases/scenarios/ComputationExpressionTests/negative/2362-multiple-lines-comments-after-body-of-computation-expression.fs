let getBlah _ =
    async {
        do! Async.Sleep 5000
        return CurrentLoanRetrieved None
        // The comments are indented before
        // return CurrentLoanRetrieved (Some { A = 14; B =  })
        // return CurrentLoanRetrievalFailed "blah balh"
    }
    |> Cmd.ofAsyncMsg
