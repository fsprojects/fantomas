let a : (unit -> int) list =
    fun () -> failwith "" : int
    |> List.singleton
    |> id
