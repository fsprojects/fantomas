type Thing =
    | Foo of msg : string
    override this.ToString() : string =
        match this with
        | Foo(ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff) ->
            ""
