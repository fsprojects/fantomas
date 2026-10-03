let drawer =
    Mui.drawer [
        // drawer.open' props.IsOpen

        drawer.children [
            Html.div [ prop.className classes.toolbar ]
            props.Items
            |> List.map (fun s ->
                Mui.listItem [
                    listItem.button true
                    match state with
                    | Some t when t = s -> listItem.selected true
                    | _ -> listItem.selected false
                    prop.text s
                    prop.onClick (fun _ -> s |> MenuItemClick |> dispatch)
                ])
            |> Mui.list
        ]
    ]
