(*---
# A `return:` attribute stays where it is written: in one list with another attribute, alone, and before another list.
---*)
[<DllImport(core); return: MarshalAs(UnmanagedType.I1)>]
extern bool ts_node_has_error(TSNode node)

[<return: MarshalAs(UnmanagedType.I1)>]
extern bool ts_node_has_error(TSNode node)

[<return: MarshalAs(UnmanagedType.I1)>]
[<DllImport(core)>]
extern bool ts_node_has_error(TSNode node)
