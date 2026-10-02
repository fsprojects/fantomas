namespace B

type Foo =
    member Item : 't -> unit when 't : comparison with set
