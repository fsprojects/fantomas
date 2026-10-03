(*---
fsharp_experimental_elmish = true
---*)
let a =
    Html.div [
        Html.h1 [ prop.text "short" ]
        Html.button [
            prop.style [ style.marginRight 5 ]
            prop.onClick (fun _ -> setCount(count + 1))
            prop.text "Increment"
        ]
    ]
