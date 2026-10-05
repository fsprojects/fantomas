(*---
fsharp_max_if_then_else_short_width = 70
---*)
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None
