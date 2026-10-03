module Foo =
    let bar () =
        let baz =
            []
            |> List.filterWith
                X
                (fun ref ->
                    if ref.Type <> "h" then
                        false
                    else

                    let m = regex.Match ref.To

                    m.Success
                    && things |> Set.contains (m.Groups.[1].ToString())
                )

        0
