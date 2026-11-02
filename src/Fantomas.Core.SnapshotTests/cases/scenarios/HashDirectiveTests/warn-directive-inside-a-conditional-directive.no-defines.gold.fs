module M

let f x =
    #if DEBUG
    #else
    x + 2
#endif
