let canConvertMemorised =
    Memoized.memoize (fun objectType ->
        ( // Include F# discriminated unions
        FSharpType.IsUnion objectType
        // and exclude the standard FSharp lists (which are implemented as discriminated unions)
        && not (
            objectType.GetTypeInfo().IsGenericType
            && objectType.GetGenericTypeDefinition() = typedefof<_ list>
        ))
        // include tuples
        || tupleAsHeterogeneousArray && FSharpType.IsTuple objectType)
