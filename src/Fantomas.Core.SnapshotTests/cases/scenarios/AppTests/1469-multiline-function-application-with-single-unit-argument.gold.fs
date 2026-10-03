namespace SomeNs

module SomeModule =
    let GetNormalAccountsPairingInfoForWatchWallet
        ()
        : Option<WatchWalletInfo> =
        let initialAbs =
            initialFeeWithAMinimumGasPriceInWeiDictatedByAvailablePublicFullNodes
                .CalculateAbsoluteValue()

        initialAbs / 100
