(*---
fsharp_max_infix_operator_expression = 20
---*)
let WebApp = route "/ping" >=> authorized >=> text "pong"
