namespace X

type TransactionType =
    | [<CompiledName "External Credit Balance Refund">] ExternalCreditBalanceRefund
    | [<CompiledName "Credit Balance Adjustment (Applied from Credit Balance)">] CreditBalanceAdjustmentAppliedFromCreditBalance
