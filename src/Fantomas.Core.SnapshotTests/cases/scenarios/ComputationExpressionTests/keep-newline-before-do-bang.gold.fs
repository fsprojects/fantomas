let private removeSubscription (log : ILogger) (req : HttpRequest) =
    log.LogInformation("Start remove-subscription")

    task {
        let origin = req.Headers.["Origin"].ToString()
        let user = Authentication.getUser log req
        let! endpoint = req.ReadAsStringAsync()
        let! managementToken = Authentication.getManagementAccessToken log
        let! existingSubscriptions = Authentication.getUserPushNotificationSubscriptions log managementToken user.Id

        do! filterSubscriptionsAndPersist managementToken user.Id existingSubscriptions origin endpoint

        return sendText "Subscription removed"
    }
