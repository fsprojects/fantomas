module Foo =
    let bar =

        match
            output.Exists
            && not (
                output.EnumerateFiles () |> Seq.isEmpty
                && onlyContainsFoobar output
            )
        with
        | true -> Error (FooBarBazError.ErrorCase output)
        | false ->

        let blah =
            let x = y
            None

        { Hi = blah } |> Ok
