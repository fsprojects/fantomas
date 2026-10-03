module private XAttrHandler =

    [<SupportedOSPlatform("macos")>]
    [<DllImport("/usr/lib/libSystem.dylib", EntryPoint = "getxattr", SetLastError = true)>]
    extern int64 private getxattrMacOs(
        string path,
        string name,
        byte[] | null value,
        uint64 size,
        uint32 position,
        int options
    )
