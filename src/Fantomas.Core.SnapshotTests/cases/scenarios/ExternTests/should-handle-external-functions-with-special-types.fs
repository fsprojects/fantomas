open System
open System.Runtime.InteropServices
open Accessibility

[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer,
    int iChildStart,
    int cChildren,
    [<Out()>] [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object [] rgvarChildren,
    int* pcObtained)