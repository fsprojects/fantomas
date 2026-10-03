type UnionTypeConverter() =
    inherit JsonConverter()
    let doRead (reader: JsonReader) = reader.Read() |> ignore

    override x.CanConvert(typ: Type) =
        let result =
            ((typ.GetInterface(typeof<System.Collections.IEnumerable>.FullName) = null)
             && FSharpType.IsUnion typ)

        result
