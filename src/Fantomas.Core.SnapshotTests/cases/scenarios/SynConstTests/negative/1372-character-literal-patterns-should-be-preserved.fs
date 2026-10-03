let f (c: char) =
    match c with
    | '\''
    | '\"'
    | '\x00'
    | '\u0000'
    | _ -> ()
