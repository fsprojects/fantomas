type Native =
    [<DllImport("user32.dll")>]
    static extern int MessageBeep(uint32 beepType)
