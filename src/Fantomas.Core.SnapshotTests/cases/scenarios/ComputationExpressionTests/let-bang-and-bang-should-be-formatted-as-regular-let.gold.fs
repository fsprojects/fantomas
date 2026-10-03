let f2 =
    async {
        // When binding, newline force-removed, which makes the whole expression
        // on the right side to be indented.
        let! r =
            match 0 with
            | _ -> () |> async.Return

        and! s =
            match 0 with
            | _ -> () |> async.Return

        return r + s
    }
