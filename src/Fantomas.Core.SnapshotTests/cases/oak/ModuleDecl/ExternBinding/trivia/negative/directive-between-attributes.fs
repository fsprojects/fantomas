#if WINDOWS
[<DllImport("kernel32.dll")>]
#else
[<DllImport("libc")>]
#endif
extern int getpid()
