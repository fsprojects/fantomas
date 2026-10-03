module Foo =

    let bar =
        baz
        |> (
            // Hi!
            match false with
            | true -> id
            | false -> id
        )
