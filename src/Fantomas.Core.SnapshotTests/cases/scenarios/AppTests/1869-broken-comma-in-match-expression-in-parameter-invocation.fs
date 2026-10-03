module Utils

type U = A of int | B of string

type C() =
    member this.M (a : int, b : string) = ()

let f () =
    let u = A 0
    do C().M(
        match u with
             | A i -> i
             | B _ -> 0
             ,
        match u with
             | A _ -> ""
             | B s -> s
    )
