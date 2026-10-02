(*---
fsharp_space_before_colon = true
fsharp_multiline_bracket_style = cramped
---*)
type Auth0User =
    { UserId : string
      AppMetaData : AppMetaData }

    static member Decoder : Decoder<Auth0User> =
        Decode.object (fun get ->
            let userId =
                get.Required.Field "user_id" Decode.string

            let metaData =
                get.Optional.Field "app_metadata" AppMetaData.Decoder
                |> Option.defaultValue ({ PushNotificationSubscriptions = [] })

            { UserId = userId
              AppMetaData = metaData })
