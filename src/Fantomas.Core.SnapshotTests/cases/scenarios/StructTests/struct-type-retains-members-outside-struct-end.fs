type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }
    end

    member x.Upper() =
        x.Name.ToUpper()

    member x.Lower() =
        x.Name.ToLower()

let n = new NameStruct("Hippo")