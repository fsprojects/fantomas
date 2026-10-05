let printListWithOffset a list1 =
    List.iter
        (((+) a)
         >> printfn "%d")
        list1
