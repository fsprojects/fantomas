(*---
fsharp_space_before_colon = true
fsharp_max_infix_operator_expression = 50
fsharp_max_array_or_list_width = 40
fsharp_multiline_bracket_style = cramped
fsharp_experimental_elmish = true
---*)
let private useLocationDetail (auth0 : Auth0Hook) (roles : RolesHook) id =
    let id = Guid.Parse(id)
    let eventCtx = React.useContext (eventContext)
    let (creatorName, setCreatorName) = React.useState<string option> (None)

    let location =
        React.useMemo ((fun () -> getLocation eventCtx.Events id), [| eventCtx.Events; id |])

    React.useEffect
        ((fun () ->
            if roles.IsEditorOrAdmin
               && not (String.IsNullOrWhiteSpace(location.Creator)) then
                auth0.getAccessTokenSilently ()
                |> Promise.bind (fun authToken ->
                    let url =
                        sprintf "%s/users/%s" Common.backendUrl (location.Creator)

                    fetch
                        url
                        [ requestHeaders [ HttpRequestHeaders.ContentType "application/json"
                                           Common.authHeader authToken
                                           Common.subscriptionHeader ] ])
                |> Promise.bind (fun res -> res.text ())
                |> Promise.iter (fun json ->
                    let usersResult = Decode.fromString nameDecoder json

                    match usersResult with
                    | Ok name -> setCreatorName (Some name)
                    | Error err -> JS.console.log err)),
         [| box roles.Roles
            box location.Creator |])

    location, creatorName
