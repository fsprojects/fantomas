namespace FicroKanSharp.Test

module TestThing =

    let ``Recursive example`` () =

        match Stream.peel rest with
        | None -> failwith "oh no"
        | Some longName ->

        longName
        |> Map.map (fun _ -> TypedTerm.force<int>)
