type Foo =
  | Bar of int
  | Baz

let foo =
  function
  | Bar (1 | 2) -> true
  | _ -> false
