(*---
fsharp_experimental_elmish = true
---*)
let a =
        Html.button [
            prop.style [ style.marginLeft 5 ]
            prop.onClick (fun _ -> setCount(count - 1))
            prop.text "Decrement"
        ]
