(*---
max_line_length = 80
---*)
let f x =
    someveryveryveryverylongexpression
    <|> if someveryveryveryverylongexpression then someveryveryveryverylongexpression else someveryveryveryverylongexpression
    <|> if someveryveryveryverylongexpression then someveryveryveryverylongexpression else someveryveryveryverylongexpression
    |> f
    