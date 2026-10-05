module InteropWithNative =
    [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
    extern void setCallbridgeSupportTarget(IntPtr newTarget)
