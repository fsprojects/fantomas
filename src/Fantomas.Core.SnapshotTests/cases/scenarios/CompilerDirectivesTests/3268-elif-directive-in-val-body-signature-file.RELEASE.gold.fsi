namespace Foobar

val x: int =
    #if DEBUG
    #elif RELEASE
    2
#else
#endif
