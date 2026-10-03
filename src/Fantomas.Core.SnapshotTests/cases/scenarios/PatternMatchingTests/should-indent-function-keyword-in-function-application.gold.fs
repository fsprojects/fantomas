let v =
    List.tryPick
        (function
        | 1 -> Some 1
        | _ -> None)
        [ 1; 2; 3 ]
