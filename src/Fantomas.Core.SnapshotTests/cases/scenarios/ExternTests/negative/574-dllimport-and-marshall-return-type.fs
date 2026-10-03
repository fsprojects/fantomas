[<DllImport("userenv.dll", SetLastError = true)>]
[<MarshalAs(UnmanagedType.Bool)>]
extern bool DestroyEnvironmentBlock(IntPtr lpEnvironment)
