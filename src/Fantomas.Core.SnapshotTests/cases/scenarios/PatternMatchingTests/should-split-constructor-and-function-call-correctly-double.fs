(*---
max_line_length = 80
fsharp_max_record_width = 80
---*)
let update msg model =
    let res =
        match msg with
        | AMessage -> { model with AFieldWithAVeryVeryVeryLooooooongName = 10 }.RecalculateTotal()
        | AnotherMessage -> model
    res
