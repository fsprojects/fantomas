module Native =

    /// <summary>Whether the parser had to guess anywhere inside this node.</summary>
    [<DllImport(core)>]
    [<return: MarshalAs(UnmanagedType.I1)>]
    extern bool ts_node_has_error(TSNode node)
