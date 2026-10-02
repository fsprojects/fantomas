#if WINDOWS
#else
[<DllImport("libc")>]
#endif
extern int getpid()
