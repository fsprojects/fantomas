#if WINDOWS
[<DllImport("kernel32.dll")>]
#else
#endif
extern int getpid()
