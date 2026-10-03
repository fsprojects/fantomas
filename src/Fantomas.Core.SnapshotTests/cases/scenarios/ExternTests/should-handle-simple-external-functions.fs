module InteropWithNative =
        [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
        extern IntPtr setCallbridgeSupportTarget(IntPtr newTarget)