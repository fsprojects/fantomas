let ilvs =
    lvs
    |> Array.toList
    |> List.filter (fun l ->
        let k, _idx = pdbVariableGetAddressAttributes l
        k = 1 (* ADDR_IL_OFFSET *) )
