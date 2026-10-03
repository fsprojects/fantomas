(*---
max_line_length = 60
---*)
    type FuseSortFunctionItem =
        abstract Item: key: string -> U2<{| ``$``: string |}, ResizeArray<{| ``$``: string; idx: float |}>> with get, set
        abstract X : int
