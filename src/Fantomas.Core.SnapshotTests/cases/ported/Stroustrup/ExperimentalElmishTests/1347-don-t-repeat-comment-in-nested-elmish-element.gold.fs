let html =
    Html.div [
        prop.className "navbar-menu"
        prop.children [
            Html.div [
                prop.className "navbar-start"
                prop.children [
                    Html.a [ prop.className "navbar-item" ]
                    (*
                    Html.a [ prop.className "navbar-item"; prop.href (baseUrl +/ "Files") ] [
                        prop.text "Files"
                    ]*)
                ]
            ]
        ]
    ]
