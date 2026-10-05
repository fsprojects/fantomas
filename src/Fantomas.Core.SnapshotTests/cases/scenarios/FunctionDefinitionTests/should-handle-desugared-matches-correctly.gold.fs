type U = X of int

let f =
    fun x ->
        match x with
        | X(x) -> x
