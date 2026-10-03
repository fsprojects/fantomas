module Lifecycle =


    let init config =
        async {
            cfg <- config

            do!
                MassTransit.init
                    cfg.LoggerFactory
                    cfg.AzureServiceBusConnStr
                    cfg.QueueName
                    cfg.LoggerFactory
                    (fun reg ->
                        reg.Consume User.handleUserInitiatedRegistration
                        reg.Consume User.handleUserUpdated
                        reg.Consume User.handleGetSessionUserIdRequest)
        }
