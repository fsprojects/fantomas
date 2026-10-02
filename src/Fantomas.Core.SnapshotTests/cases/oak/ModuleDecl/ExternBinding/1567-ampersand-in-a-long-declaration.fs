(*---
# A declaration too long for one line puts each parameter on its own line, `&` included.
---*)
[<DllImport("Kernel32.dll", SetLastError = true)>]
extern bool GetFileInformationByHandleEx(IntPtr hFile, FILE_INFO_BY_HANDLE_CLASS infoClass, [<Out>] FILE_NAME_INFO& info, uint32 dwBufferSize)
