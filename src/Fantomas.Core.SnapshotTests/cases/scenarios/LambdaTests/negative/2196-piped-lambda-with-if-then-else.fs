(*---
fsharp_max_infix_operator_expression = 45
---*)
let dayOfWeekToNum (d: DayOfWeek) =
    int d
    |> fun x -> if x = 0 then 7 else x
    |> DayNum
