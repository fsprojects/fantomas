let options =
    jsOptions<Vis.Options>(fun o ->
        o.autoResize <- Some true
        o.edges <- Some(jsOptions<Vis.EdgeOptions>(fun e -> e.arrows <- Some <| U2.Case1 "to"))

        o.interaction <-
            Some(
                createObj
                    [ "hover" ==> true
                      "zoomView" ==> true
                      "hoverConnectedEdges" ==> false ]
            )

        o.layout <- Some(createObj [ "randomSeed" ==> 0 ])

        let hierOpts dir =
            createObj
                [ "enabled" ==> true
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
