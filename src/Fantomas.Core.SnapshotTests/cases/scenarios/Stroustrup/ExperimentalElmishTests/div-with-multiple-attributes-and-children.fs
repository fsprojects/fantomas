(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_elmish = true
---*)
let d =
    div [ ClassName "container"; OnClick (fun _ -> printfn "meh") ] [
        span [] [str "foo"]
        code [] [str "bar"]
    ]
