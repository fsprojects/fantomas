let options =
    jsOptions<Vis.Options>(fun o ->
        let layout =
            match opts.Layout with
            | Graph.Free -> createObj []
            | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
            | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

        o.layout <- Some layout)
