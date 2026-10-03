let d =
    div [
        ClassName "container"
        OnClick(fun _ -> printfn "meh")
    ] [
        span [] [ str "foo" ]
        code [] [ str "bar" ]
    ]
