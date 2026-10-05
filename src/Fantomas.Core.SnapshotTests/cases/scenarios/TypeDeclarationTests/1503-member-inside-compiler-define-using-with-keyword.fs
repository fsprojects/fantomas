(*---
indent_size = 2
---*)
type A =
  | B of int
  | C

#if DEBUG
  with
    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
#endif
