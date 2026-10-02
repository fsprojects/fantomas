namespace Foobar

val x: int =
    #if DEBUG
    1
#elif RELEASE
#else
#endif
