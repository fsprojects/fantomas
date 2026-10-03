module Foo =
    let bar =
        []
        |> List.choose
            (function
             | _ -> "")
