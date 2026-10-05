(*---
max_line_length = 60
---*)
let first =
    (line.Split([| ":" |], StringSplitOptions.RemoveEmptyEntries)).Length
