let u = ""

match!
    match! u with
    | null -> ""
    | s -> s
with
| "" -> x
| _ -> failwith ""
