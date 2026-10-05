module Foo =

    let bar () =
        []
        |> Seq.fold
            (fun (a, b) ->
                let blah =
                    fieldInfos
                    |> Seq.groupBy (fun fi -> fi.Name)
                    |> Seq.filter (fst >> foo >> not)
                    |> Seq.choose (fun (name, fieldInfos) ->
                        let fieldTypes =
                            fieldInfos
                            |> Seq.map (fun fi -> TypeId fi.TypeInfo.Id)
                            |> Seq.distinct
                            |> Seq.toList

                        match fieldTypes with
                        | [ fieldType ] -> // hi!
                            let parents = fieldInfos |> Seq.cache
                            Some(name, fieldType, parents)
                        | _ -> // differing
                            None
                    )

                ()
            )
            meh
