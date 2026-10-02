type A =
    | B of int
    | C

#if DEBUG
    member this.GetB =
        match this with
        | B x -> x
        | _ -> failwith "shouldn't happen"
#endif
