type Foo =
    static member Create a = {| Name = "Isaac" |}

type Bar() =
    member _.Create(a, b) = ()

let bar = Bar ()

type Thing =
    static member Stuff = 123

let x =
    {| Foo.Create (
           [
               bar.Create (Thing.Stuff, Thing.Stuff)
           ]
       ) with
        Age = 41
    |}
