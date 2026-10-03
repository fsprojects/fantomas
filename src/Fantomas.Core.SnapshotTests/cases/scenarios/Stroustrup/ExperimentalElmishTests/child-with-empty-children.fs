(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_elmish = true
---*)
let commands dispatch =
    Button.button
        [ Button.Color Primary
          Button.Custom
              [ ClassName "rounded-0"
                OnClick(fun _ -> dispatch GetTrivia) ] ]
        [ i [ ClassName "fas fa-code mr-1" ] []
          str "Get trivia" ]
