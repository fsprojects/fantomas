type Thing =
    | Foo of msg: string
    override this.ToString() =
        match this with
        | Foo(ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) ->
            ""
