(*---
max_line_length = 60
---*)
let args =
    match args with
    | [| LongPatIndentifierOne | LongPatIndentifierTwo | LongPatIndentifierThree |] ->
        args
    | _ -> failwith "meh"
