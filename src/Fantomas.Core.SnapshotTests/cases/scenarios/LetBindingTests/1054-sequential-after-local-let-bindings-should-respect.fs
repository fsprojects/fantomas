let merge a b =
    let aChunks = splitWhenHash a
    let bChunks = splitWhenHash b

    if List.length aChunks <> List.length bChunks then
        Dbg.print (aChunks, bChunks)
        failwithf """Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back togheter. Please raise an issue at https://github.com/fsprojects/fantomas/issues."""

    List.zip aChunks bChunks
    |> List.map (fun (a', b') ->
        let la = lengthWithoutSpaces a'
        let lb = lengthWithoutSpaces b'
        if la <> lb then
            if la > lb then a' else b'
        else
            if String.length a' < String.length b' then a' else b'
    )

    |> String.concat Environment.NewLine
