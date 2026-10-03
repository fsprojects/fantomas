/// Closes the handle.
// Only on Windows.
[<DllImport("kernel32.dll")>]
extern bool CloseHandle(nativeint handle)
