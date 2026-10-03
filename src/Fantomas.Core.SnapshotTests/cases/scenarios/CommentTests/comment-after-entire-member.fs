type Meh () =
    member x.IsComInterop =
        x.Attributes &&& TypeAttributes.Import
        <> enum 0 (* Class or interface generated for COM interop *)
