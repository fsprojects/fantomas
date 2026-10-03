(*---
fsharp_multiline_bracket_style = stroustrup
---*)
let view (model: Model) dispatch =
    View.ContentPage(
        appearing=(fun () -> dispatch PageAppearing),
        title=model.Planet.Info.Name,
        backgroundColor=Color.Black,
        content=["....long line....................................................................................................."]
    )
