namespace GeeTower.Tests.EndToEnd

module WatcherTests =

    let CanRevokeAnIllegalCommitmentTx () =
        let lndAddress = obj ()

        let config =
            { GeeTower.Backend.Configuration.GetTestingConfig(
                  lndAddress.ToString()
              ) with
                BitcoinRpcUser = "btc"
            }

        ()
