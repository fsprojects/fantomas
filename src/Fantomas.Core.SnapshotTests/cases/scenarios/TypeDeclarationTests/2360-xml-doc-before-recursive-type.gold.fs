module Primitives =
    type BlockHeight = BlockHeight of uint32

    /// **Description**
    ///
    /// 16bit relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and
#if !NoDUsAsStructs
        [<Struct>]
#endif
        BlockHeightOffset16 = BlockHeightOffset16 of uint16
