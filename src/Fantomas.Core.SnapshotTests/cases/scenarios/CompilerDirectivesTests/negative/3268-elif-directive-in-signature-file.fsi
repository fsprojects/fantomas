namespace Foobar

#if DEBUG
val x: int
#elif RELEASE
val y: int
#else
val z: int
#endif
