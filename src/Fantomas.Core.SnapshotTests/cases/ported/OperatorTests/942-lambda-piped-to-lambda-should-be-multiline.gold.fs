let r (f: 'a -> 'b) (a: 'a) : 'b =
    fun () -> f a
    |> fun f -> f ()
