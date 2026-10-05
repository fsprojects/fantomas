(*---
max_line_length = 40
---*)
let _ =
    List.maaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaap (fun _ -> @"a
b"     )
       |> List.length
