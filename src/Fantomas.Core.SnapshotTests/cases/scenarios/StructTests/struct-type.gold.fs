type NameStruct =
    struct
        val Name: string
        new(name) = { Name = name }

        member x.Upper() = x.Name.ToUpper()

        member x.Lower() = x.Name.ToLower()
    end

let n = new NameStruct("Hippo")
