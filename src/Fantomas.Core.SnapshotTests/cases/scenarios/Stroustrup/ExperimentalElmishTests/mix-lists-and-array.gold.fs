let view dispatch model =
    div [| Class "container" |] [
        h1 [] [| str "my title" |]
        button [| OnClick(fun _ -> dispatch Msg.Foo) |] [ str "click me" ]
    ]
