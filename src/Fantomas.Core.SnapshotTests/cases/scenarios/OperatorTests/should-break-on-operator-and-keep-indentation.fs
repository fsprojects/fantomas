(*---
max_line_length = 80
fsharp_max_infix_operator_expression = 60
---*)
let pattern =
    (x + y)
      .Replace(seperator + "**" + seperator, replacementSeparator + "(.|?" + replacementSeparator + ")?" )
      .Replace("**" + seperator, ".|(?<=^|" + replacementSeparator + ")" )
    