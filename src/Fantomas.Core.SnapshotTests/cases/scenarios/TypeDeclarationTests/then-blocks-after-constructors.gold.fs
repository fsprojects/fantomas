type Person(nameIn: string, idIn: int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object."

    member this.Name
        with get () = name
        and set (v) = name <- v

    member this.ID
        with get () = id
        and set (v) = id <- v

    new() =
        Person("Invalid Name", -1)
        then printfn "Created an invalid person object."
