module M

module N =
#nowarn "25"
    let x = 1

let f x =
    let y = 1
#nowarn "25"
    match x with
    | 1 ->
#warnon "25"
        y
    | _ -> 0

type T() =
    member _.A = 1
#nowarn "25"
    member _.B = 2

let xs =
    [
        1
#nowarn "25" // a comment
        2
    ]
