module ElectrumClient =

    let private Init (fqdn: string) (port: uint32) : Async<StratumClient> =
        let PROTOCOL_VERSION_SUPPORTED = Version "1.4"

        async {
            let! versionSupportedByServer =
                try
                    stratumClient.ServerVersion
                        CLIENT_NAME_SENT_TO_STRATUM_SERVER_WHEN_HELLO
                        PROTOCOL_VERSION_SUPPORTED
                with :? ElectrumServerReturningErrorException as except when
                    except.Message.EndsWith(
                        PROTOCOL_VERSION_SUPPORTED.ToString()
                    ) ->

                    failwith "xxx"

            return stratumClient
        }
