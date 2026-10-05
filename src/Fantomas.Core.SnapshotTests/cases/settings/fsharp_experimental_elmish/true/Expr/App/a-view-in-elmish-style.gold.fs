let view model dispatch =
    div [ ClassName "container" ] [
        h1 [] [ str "Counter" ]
        button [ OnClick(fun _ -> dispatch Increment) ] [ str "+" ]
    ]
