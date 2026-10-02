module Foo =
    let Bar () =
        if x then
            match foo with
            | {
                  Bar = true
                  Baz = _
              } ->
                failwith "xxx"
            | _ -> None
