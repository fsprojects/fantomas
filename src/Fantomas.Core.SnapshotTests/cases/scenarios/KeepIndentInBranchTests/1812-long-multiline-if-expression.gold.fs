module Foo =
    let bar =

        if
            output.Exists
            && not (
                output.EnumerateFiles () |> Seq.isEmpty
                && onlyContainsFoobar output
            )
        then
            Error (FooBarBazError.ErrorCase output)
        else

        let blah =
            let x = y
            None

        { Hi = blah } |> Ok
