type IWSAMTest<'e> =
    static abstract member Test : int -> 'e
    static abstract member Zero : 'e
    abstract member Value : int
