(*---
fsharp_space_before_colon = true
fsharp_multiline_bracket_style = cramped
---*)
let private update onSubmit msg model =
    match msg with
    | UpdateName n -> ({ model with Name = n } : Model), Cmd.none
    | UpdatePrice p -> { model with Price = p }, Cmd.none
    | UpdateCurrency c -> { model with Currency = c }, Cmd.none
    | UpdateLocation (lat, lng) ->
        { model with
              Latitude = lat
              Longitude = lng },
        Cmd.none
    | UpdateIsDraft d -> { model with IsDraft = d }, Cmd.none
    | UpdateRemark r -> { model with Remark = r }, Cmd.none
    | UpdateLocationError isError ->
        let errors =
            if isError then
                model.Errors
                |> Map.add "distance" [ "De gekozen locatie is te ver van jouw locatie! Das de bedoeling niet veugel." ]
            else
                Map.remove "distance" model.Errors

        { model with Errors = errors }, Cmd.none
