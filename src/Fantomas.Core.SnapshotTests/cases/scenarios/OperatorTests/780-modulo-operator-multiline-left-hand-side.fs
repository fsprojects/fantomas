(*---
fsharp_max_infix_operator_expression = 50
---*)
let hasUnEvenAmount regex line = (Regex.Matches(line, regex).Count - Regex.Matches(line, "\\\\" + regex).Count) % 2 = 1
