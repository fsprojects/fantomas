type Foo() =
    member x.Get = 1

    member x.Set
        with private set (v: int) = value <- v

    member x.GetSet
        with internal get () = value
        and private set (v: bool) = value <- v

    member x.GetI
        with internal get (key1, key2) = false

    member x.SetI
        with private set (key1, key2) value = ()

    member x.GetSetI
        with internal get (key1, key2) = true
        and private set (key1, key2) value = ()
