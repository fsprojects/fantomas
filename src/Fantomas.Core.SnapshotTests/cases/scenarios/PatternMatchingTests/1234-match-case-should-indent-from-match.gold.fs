let foo x =
  match x with
  | Some x -> x
  | None ->
    let x = 123
    let y = x * x * (x + 1)
    x
