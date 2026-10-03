match x with
| _ ->
        try
            somethingElse ()
        with
        | e -> printfn "failure %A" e
--*-- bar
