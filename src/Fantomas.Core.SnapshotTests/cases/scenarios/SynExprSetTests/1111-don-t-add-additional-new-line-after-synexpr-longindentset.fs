(*---
fsharp_max_infix_operator_expression = 50
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
---*)
        let options =
            jsOptions<Vis.Options> (fun o ->
                o.autoResize <- Some true
                o.edges <- Some(jsOptions<Vis.EdgeOptions> (fun e -> e.arrows <- Some <| U2.Case1 "to"))

                o.interaction <-
                    Some
                        (createObj [ "hover" ==> true
                                     "zoomView" ==> true
                                     "hoverConnectedEdges" ==> false ])

                o.layout <- Some(createObj [ "randomSeed" ==> 0 ])

                let hierOpts dir =
                    createObj [ "enabled" ==> true
                                "levelSeparation" ==> 170
                                "nodeSpacing" ==> 100
                                "treeSpacing" ==> 100
                                "direction" ==> dir ]

                let layout =
                    match opts.Layout with
                    | Graph.Free -> createObj []
                    | Graph.HierarchicalLeftRight -> createObj [ "hierarchical" ==> hierOpts "LR" ]
                    | Graph.HierarchicalUpDown -> createObj [ "hierarchical" ==> hierOpts "UD" ]

                o.layout <- Some layout)
