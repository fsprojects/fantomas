type TorDirectory =
    private
        { NetworkStatus: NetworkStatusDocument }

    static member Bootstrap(nodeEndPoint: IPEndPoint) =
        async {
            return
                { TorDirectory.NetworkStatus = NetworkStatusDocument.Parse consensusStr
                  ServerDescriptors = Map.empty
                // comment
                }
        }
