type [<AllowNullLiteral>] ArrayBuffer =
    abstract byteLength: int
    abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer
