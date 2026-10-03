let internal ImageLoadResilient (f: unit -> 'a) (tidy: unit -> 'a) =
    try
        f ()
    with
    | :? BadImageFormatException
    | :? ArgumentException
    | :? IOException -> tidy ()
