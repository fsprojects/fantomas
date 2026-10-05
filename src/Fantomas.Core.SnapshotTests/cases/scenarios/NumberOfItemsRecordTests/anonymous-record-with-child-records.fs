(*---
fsharp_record_multiline_formatter = number_of_items
fsharp_multiline_bracket_style = cramped
---*)
let anonRecord =
    {| A = {| A1 = "string";A2LongerIdentifier = "foo" |};
       B = {| B1 = 7 |}
       C= { C1 = "foo"; C2LongerIdentifier = "bar"}
       D = { D1 = "bar" } |}
