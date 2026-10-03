(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_elmish = true
---*)
let view =
    div [ ClassName "container" ] [
        h1 [] [ str "A heading 1" ]
        p [] [ str "A paragraph" ]
    ]
