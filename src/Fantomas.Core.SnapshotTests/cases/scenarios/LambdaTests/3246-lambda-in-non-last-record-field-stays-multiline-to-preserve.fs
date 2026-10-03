type Rec = {
    A: int
    B: int -> int
    C: int
}

let test () : Rec =
    {
        A = 1
        B = fun x -> x + 1
        C = 3
    }
