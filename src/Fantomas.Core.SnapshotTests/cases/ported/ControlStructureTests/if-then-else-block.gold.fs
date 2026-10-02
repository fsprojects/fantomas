let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred (head) then Some(head) else tryFindMatch pred tail
    | [] -> None

let test x y =
    if x = y then "equals"
    elif x < y then "is less than"
    else if x > y then "is greater than"
    else "Don't know"

if age < 10 then
    printfn "You are only %d years old and already learning F#? Wow!" age
