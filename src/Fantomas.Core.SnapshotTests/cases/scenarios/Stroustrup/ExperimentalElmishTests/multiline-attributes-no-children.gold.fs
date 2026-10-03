let a =
    button [
        ClassName "destroy"
        OnClick(fun _ -> Delete todo.id |> dispatch)
    ] []
