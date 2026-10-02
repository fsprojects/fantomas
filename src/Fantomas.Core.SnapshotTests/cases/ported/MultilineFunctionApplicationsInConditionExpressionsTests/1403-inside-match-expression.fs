(*---
max_line_length = 40
---*)
let foo () =
    match b.TryGetValue (longlonglonglonglong, b) with
    | true, i -> Some i
    | false, _ -> failwith ""
