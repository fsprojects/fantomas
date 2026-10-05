type T =
    [<DllImport("user32.dll")>]
    static (* keep *) extern int F(int x)
