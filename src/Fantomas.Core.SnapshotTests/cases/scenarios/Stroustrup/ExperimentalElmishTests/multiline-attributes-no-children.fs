(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_elmish = true
---*)
let a =
               button [ ClassName "destroy"
                        OnClick(fun _-> Delete todo.id |> dispatch) ]
                      []
