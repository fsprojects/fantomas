(*---
fsharp_max_infix_operator_expression = 50
---*)
if
        FOOQueryUserToken (uint32 activeSessionId, &token) <> 0u
      then
        Some x
      else
        None
