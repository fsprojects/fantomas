(*---
max_line_length = 60
---*)
match
    (unbox<{| __proto__: {| ChooseAsync: (('T -> Async<_ option>) -> AsyncSeq<_>) option |} option |}> (
        source'
    ))
    .__proto__ with
| Some proto when proto.ChooseAsync.IsSome ->
    source'.ChooseAsync f
| _ ->
    asyncSeq {
        for itm in source do
            let! v = f itm

            match v with
            | Some v -> yield v
            | _ -> ()
    }
