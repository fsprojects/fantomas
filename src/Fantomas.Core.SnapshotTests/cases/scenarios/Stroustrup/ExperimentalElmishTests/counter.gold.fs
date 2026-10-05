let view model dispatch =
    div [] [
        button [ OnClick(fun _ -> dispatch Decrement) ] [ str "-" ]
        div [] [ str (sprintf "%A" model) ]
        button [ OnClick(fun _ -> dispatch Increment) ] [ str "+" ]
    ]
