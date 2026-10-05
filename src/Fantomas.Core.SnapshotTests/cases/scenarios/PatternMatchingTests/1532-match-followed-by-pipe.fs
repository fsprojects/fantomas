(*---
indent_size = 2
fsharp_max_infix_operator_expression = 50
---*)
match x with
| Foo f -> []
| Bar x ->
            "\n"
            + columnHeadersText
            + "\n"
            + seprator
            + "\n"
            + itemsText
|> Some
