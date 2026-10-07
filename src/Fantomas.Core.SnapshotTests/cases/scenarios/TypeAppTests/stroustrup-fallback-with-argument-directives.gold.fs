f<
    {|
        a: int
    |},
#if DEBUG
    int
        -> int
        -> int
        -> string
#else
    {|
        b: bool
    |} list
#endif
>
