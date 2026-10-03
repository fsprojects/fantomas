let rec printList l =
    match l with
    | head :: tail ->
        printf "%d " head
        printList tail
    | [] -> printfn ""

let listLength list =
    match list with
    | [] -> 0
    | [ _ ] -> 1
    | [ _; _ ] -> 2
    | [ _; _; _ ] -> 3
    | _ -> List.length list
