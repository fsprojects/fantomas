(*---
# The two attribute lists of a parameter are joined into one, and `System.Object []` loses its space.
---*)
[<DllImport("oleacc.dll")>]
extern int AccessibleChildren(
    IAccessible paccContainer,
    int iChildStart,
    int cChildren,
    [<Out()>] [<MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 4s)>] System.Object [] rgvarChildren,
    int* pcObtained)
