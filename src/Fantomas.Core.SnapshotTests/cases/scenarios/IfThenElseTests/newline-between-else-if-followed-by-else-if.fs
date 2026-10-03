module String =
    let merge a b =
            if la <> lb then
                if la > lb then a' else b'
            else
                if String.length a' < String.length b' then a' else if String.length a' > String.length b' then b' else b'
