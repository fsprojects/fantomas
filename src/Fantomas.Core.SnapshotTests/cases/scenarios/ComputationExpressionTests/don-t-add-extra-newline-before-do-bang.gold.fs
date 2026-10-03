let sendPushNotifications =
    allSubscriptions
    |> List.map (fun (user, subscriptions) ->
        subscriptions
        |> List.filter (fun s -> s.Origin = origin)
        |> List.map (fun s ->
            task {
                try
                    let ps = PushSubscription(s.Endpoint, s.P256DH, s.Auth)

                    do! webPushClient.SendNotificationAsync(ps, payload, vapidDetails)
                with :? WebPushException as wpex ->
                    log.LogError(sprintf "Couldn't send notification to %s, %A" user.UserId wpex)

                    do!
                        filterSubscriptionsAndPersistLongLongLongLongLongLongLongLongLong
                            managementToken
                            user.UserId
                            subscriptions
                            s.Origin
                            s.Endpoint
            }
            :> Task)
        |> Task.WhenAll)
