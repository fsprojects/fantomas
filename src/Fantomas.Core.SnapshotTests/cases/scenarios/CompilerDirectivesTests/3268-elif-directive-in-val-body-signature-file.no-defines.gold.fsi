namespace Foobar

val x: int =
    #if DEBUG
    #elif RELEASE
    #else
    3
#endif
