(*---
fsharp_max_if_then_else_short_width = 75
fsharp_max_value_binding_width = 30
fsharp_max_function_binding_width = 30
---*)
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None