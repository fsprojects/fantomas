(*---
fsharp_max_value_binding_width = 50
fsharp_max_function_binding_width = 50
---*)
        let options =
            jsOptions<Vis.Options> (fun o ->
                let layout =
                    match opts.Layout with
                    | Graph.Free -> createObj []
                    | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
                    | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

                o.layout <- Some layout)
