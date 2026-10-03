(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
let anonRecord =
    {| A = {| A1 = "string";A2LongerIdentifier = "foo" |};
       B = {| B1 = 7 |}
       C= { C1 = "foo"; C2LongerIdentifier = "bar"}
       D = { D1 = "bar" } |}
