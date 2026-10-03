(*---
# Formatting drops the `static` of the extern, which changes the syntax tree.
---*)
type Native =
    [<DllImport("user32.dll")>]
    static extern int MessageBeep(uint32 beepType)
