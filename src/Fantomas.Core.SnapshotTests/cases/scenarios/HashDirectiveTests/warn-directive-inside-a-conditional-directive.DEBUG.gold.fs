module M

let f x =
    #if DEBUG
#nowarn "25"
    x + 1
#else
#endif
