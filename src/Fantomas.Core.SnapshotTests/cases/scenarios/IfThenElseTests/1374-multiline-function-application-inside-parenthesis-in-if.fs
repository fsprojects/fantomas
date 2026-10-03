(*---
max_line_length = 80
---*)
module UtxoCoinAccount =
    let internal SendPayment
        (account: NormalUtxoAccount)
        (txMetadata: TransactionMetadata)
        (destination: string)
        (amount: TransferAmount)
        (password: string)
        =
        if (baseAccount.PublicAddress.Equals (destination, StringComparison.InvariantCultureIgnoreCase)) then
            raise DestinationEqualToOrigin
