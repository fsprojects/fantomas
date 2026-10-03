(*---
fsharp_max_value_binding_width = 120
---*)
type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }

        member x.Upper() =
            x.Name.ToUpper()

        member x.Lower() =
            x.Name.ToLower()
    end

let n = new NameStruct("Hippo")