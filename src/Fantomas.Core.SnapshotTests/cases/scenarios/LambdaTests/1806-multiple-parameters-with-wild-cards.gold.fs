module Foo =
    let bar () =
        { Foo =
            blah
            |> Struct.map (fun _ (a, _, _) -> filterBackings a) }
