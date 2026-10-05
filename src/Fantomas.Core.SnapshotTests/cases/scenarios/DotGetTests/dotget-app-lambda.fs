let getColl =
    GetCollection(fun _ parser ->
        let x = 1
        x
    ).ToString()

let getColl2 =
    GetCollection(fun parser ->
        let x = 2
        x
    ).ToString()

let getColl3 =
    GetCollection(fun _ parser ->
        let x = 3
        x
    ).Foo

let getColl4 =
    GetCollection(fun parser ->
        let x = 4
        x
    ).Foo
