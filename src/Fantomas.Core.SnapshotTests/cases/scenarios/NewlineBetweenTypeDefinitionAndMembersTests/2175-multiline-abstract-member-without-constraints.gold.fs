type FuseSortFunctionItem =
    abstract Item:
        key: string ->
            U2<
                {| ``$``: string |},
                ResizeArray<{| ``$``: string; idx: float |}>
             > with get, set

    abstract X: int
