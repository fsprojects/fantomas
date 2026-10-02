(*---
# Parameters that fit go on one line.
---*)
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern int GetWindowLong(
    System.IntPtr hwnd,
    int index
)
