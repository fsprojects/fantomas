namespace B

type Foo =
    | Bar of int
    member Item : unit -> int with get
