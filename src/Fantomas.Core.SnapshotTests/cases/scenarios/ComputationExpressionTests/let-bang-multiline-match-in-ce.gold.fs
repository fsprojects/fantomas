let rec runPendingJobs () =
    task {
        let! jobToRun = checkForJob ()

        match jobToRun with
        | None -> return ()
        | Some pendingJob ->
            do! pendingJob ()
            return! runPendingJobs ()
    }
