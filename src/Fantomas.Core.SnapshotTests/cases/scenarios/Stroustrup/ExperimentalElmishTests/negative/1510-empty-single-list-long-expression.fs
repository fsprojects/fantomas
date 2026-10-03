(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_max_array_or_list_width = 20
fsharp_multi_line_lambda_closing_newline = true
fsharp_experimental_elmish = true
---*)
[<ReactComponent>]
let Dashboard () =
    Html.div [
        Html.div []
        Html.div [
            Html.text "hola muy buenas"
        ]
    ]
