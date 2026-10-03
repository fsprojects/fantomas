type Box() =
    let mutable value = 0
    member _.Value with get () = value and set v = value <- v
