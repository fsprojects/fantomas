(*---
# Both blank lines stay, though `with` between them goes.
---*)
type A =
  | B of int
  | C

  with

    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
