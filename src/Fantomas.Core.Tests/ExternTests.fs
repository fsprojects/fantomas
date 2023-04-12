module Fantomas.Core.Tests.ExternTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``attribute above extern keyword, 562`` () =
    formatAST
        false
        """
module C =
  [<DllImport("")>]
  extern IntPtr f()
"""
        config
    |> prepend newline
    |> should
        equal
        """
module C =
    [<DllImport("")>]
    extern IntPtr f()
"""

[<Test>]
let ``& in extern function declaration, 1567`` () =
    formatSourceString
        false
        """
module Foo =
    module Bar =
        [<DllImport("Kernel32.dll", SetLastError = true)>]
        extern bool GetFileInformationByHandleEx(IntPtr hFile, FILE_INFO_BY_HANDLE_CLASS infoClass, [<Out>] FILE_NAME_INFO& info, uint32 dwBufferSize)
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    module Bar =
        [<DllImport("Kernel32.dll", SetLastError = true)>]
        extern bool GetFileInformationByHandleEx(
            IntPtr hFile,
            FILE_INFO_BY_HANDLE_CLASS infoClass,
            [<Out>] FILE_NAME_INFO& info,
            uint32 dwBufferSize
        )
"""

[<Test>]
let ``should handle external functions`` () =
    formatSourceString
        false
        """[<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
extern ReturnCode  GetParent (System.IntPtr inRef, byref outParentRef)"""
        config
    |> prepend newline
    |> should
        equal
        """
[<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
extern ReturnCode GetParent(System.IntPtr inRef, byref outParentRef)
"""

[<Test>]
let ``should handle simple external functions`` () =
    formatSourceString
        false
        """module InteropWithNative =
        [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
        extern IntPtr setCallbridgeSupportTarget(IntPtr newTarget)"""
        config
    |> prepend newline
    |> should
        equal
        """
module InteropWithNative =
    [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
    extern IntPtr setCallbridgeSupportTarget(IntPtr newTarget)
"""

[<Test>]
let ``should handle external functions with void return type`` () =
    formatSourceString
        false
        """module InteropWithNative =
        [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
        extern void setCallbridgeSupportTarget(IntPtr newTarget)"""
        config
    |> prepend newline
    |> should
        equal
        """
module InteropWithNative =
    [<DllImport(@"__Internal", CallingConvention = CallingConvention.Cdecl)>]
    extern void setCallbridgeSupportTarget(IntPtr newTarget)
"""

[<Test>]
let ``should handle external functions with fully-qualified attributes`` () =
    formatSourceString
        false
        """[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern int GetWindowLong(System.IntPtr hwnd, int index)"""
        config
    |> prepend newline
    |> should
        equal
        """
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
extern int GetWindowLong(System.IntPtr hwnd, int index)
"""

[<Test>]
let ``should handle external functions with special types`` () =
    formatSourceString
        false
        """open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer,
    int iChildStart,
    int cChildren,
    [<Out()>] [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object [] rgvarChildren,
    int* pcObtained)"""
        config
    |> prepend newline
    |> should
        equal
        """
open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer,
    int iChildStart,
    int cChildren,
    [<Out; MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object[] rgvarChildren,
    int* pcObtained
)
"""

[<Test>]
let ``DllImport and Marshall return type, 574`` () =
    formatSourceString
        false
        """
[<DllImport("userenv.dll", SetLastError = true)>]
[<MarshalAs(UnmanagedType.Bool)>]
extern bool DestroyEnvironmentBlock(IntPtr lpEnvironment)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<DllImport("userenv.dll", SetLastError = true)>]
[<MarshalAs(UnmanagedType.Bool)>]
extern bool DestroyEnvironmentBlock(IntPtr lpEnvironment)
"""

[<Test>]
let ``should keep access modifiers in extern declaration, 1213`` () =
    formatSourceString
        false
        """
[<DllImport("kernel32.dll")>]
extern UIntPtr private GetProcessHeap()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<DllImport("kernel32.dll")>]
extern UIntPtr private GetProcessHeap()
"""

[<Test>]
let ``extern C-like parameters, 1216`` () =
    formatSourceString
        false
        """
extern int M1(int)

extern int M2(int i)

extern int M3([<A>] int i)

extern int M4(int[] i)

extern int M5(int* i)

extern int M5(int& i)

extern int M5(void* i)

extern int M6(void* i1, void* i2)
"""
        config
    |> prepend newline
    |> should
        equal
        """
extern int M1(int)

extern int M2(int i)

extern int M3([<A>] int i)

extern int M4(int[] i)

extern int M5(int* i)

extern int M5(int& i)

extern int M5(void* i)

extern int M6(void* i1, void* i2)
"""

[<Test>]
let ``extern with void return type, 1215`` () =
    formatSourceString
        false
        """
extern void GetProcessHeap()
extern [<A>] void GetProcessHeap2()
"""
        config
    |> prepend newline
    |> should
        equal
        """
extern void GetProcessHeap()
extern [<A>] void GetProcessHeap2()
"""

[<Test>]
let ``extern declaration inside type declaration, 1214`` () =
    formatSourceString
        false
        """
type T() =
  [<DllImport("kernel32.dll")>]
  extern UIntPtr private GetProcessHeap()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T() =
    [<DllImport("kernel32.dll")>]
    extern UIntPtr private GetProcessHeap()
"""
