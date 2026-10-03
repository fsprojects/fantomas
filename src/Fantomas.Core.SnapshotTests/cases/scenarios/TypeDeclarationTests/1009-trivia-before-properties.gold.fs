type Box() =
    let mutable color: string = null

    // A Box has a color property with get and set.
    member x.Color
        with get () = color
        and set (c) = color <- c

    member x.Color2
        // A Box has a color property with get and set.
        with get () = color
        and set (c) = color <- c

    // If there's no get/set, the comment is preserved
    member x.hello = "world"
