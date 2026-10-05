(*---
max_line_length = 80
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
namespace GeeTower.Tests.EndToEnd

module WatcherTests =

    let CanRevokeAnIllegalCommitmentTx () =
        let lndAddress = obj()

        let config = {
            GeeTower.Backend.Configuration.GetTestingConfig (lndAddress.ToString())
            with
                BitcoinRpcUser = "btc"
        }

        ()
