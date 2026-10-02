(*---
# Two independent defines: one gold per define combination, and the merged one.
---*)
type Channel =
    | Stable
#if DEBUG
    | Debug
#endif
#if TRACE
    | Trace
#endif
