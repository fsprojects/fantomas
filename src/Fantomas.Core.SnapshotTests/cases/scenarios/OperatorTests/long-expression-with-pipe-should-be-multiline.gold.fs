let a =
    List.init 40 (fun i -> generateThing i a)
    |> List.map mapThingToOtherThing
