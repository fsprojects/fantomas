let args =
    match args with
    | [ LongPatIndentifierOne
         | LongPatIndentifierTwo
         | LongPatIndentifierThree ] ->
        args
    | _ -> failwith "meh"
