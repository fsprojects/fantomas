(*---
# `void` as the return type, without and with an attribute on it. The attribute stays on the type.
---*)
extern void GetProcessHeap()
extern [<A>] void GetProcessHeap2()
