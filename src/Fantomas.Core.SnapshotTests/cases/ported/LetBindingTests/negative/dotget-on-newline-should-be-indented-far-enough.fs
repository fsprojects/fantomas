(*---
max_line_length = 70
fsharp_max_value_binding_width = 60
---*)
let tomorrow =
    DateTimeOffset(n.Year, n.Month, n.Day, 0, 0, 0, n.Offset)
        .AddDays(1.)
