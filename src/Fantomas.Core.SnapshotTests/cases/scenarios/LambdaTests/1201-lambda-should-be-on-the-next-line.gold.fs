let printListWithOffset a list1 =
    List.iter (fun elem -> printfn "%d" (a + elem)) list1

// OK if lambda body is long enough
let printListWithOffset a list1 =
    List.iter
        (fun elem ->
            // OK if lambda body is long enough
            printfn "%d" (a + elem))
        list1
