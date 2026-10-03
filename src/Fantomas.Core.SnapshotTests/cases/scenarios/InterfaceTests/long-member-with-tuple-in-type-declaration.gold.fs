type IFoo =
    abstract Bar :
        [<Path "bar">] bar : string *
        [<Path "baz">] baz : string ->
            Task<Foo>
