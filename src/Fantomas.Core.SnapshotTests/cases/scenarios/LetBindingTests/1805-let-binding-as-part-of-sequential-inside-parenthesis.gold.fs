module Foo =
    let foo =
        lazy
            (if not <| bar then
                 raise
                 <| Exception "Very very very very very very very very very very very very very very long"

             let ret = false

             if ret then "foo" else "bar"
             |> log.Info

             ret)
