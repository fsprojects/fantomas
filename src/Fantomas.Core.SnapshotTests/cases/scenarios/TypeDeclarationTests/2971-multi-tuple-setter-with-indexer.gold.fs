type MyArray3() =
    member _.Item
        with get (x: int, y: int, z: int) = ()
        and set (x: int, y: int, z: int) v = ()
