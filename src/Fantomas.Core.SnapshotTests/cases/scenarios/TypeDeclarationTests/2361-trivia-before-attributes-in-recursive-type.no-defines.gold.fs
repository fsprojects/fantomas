module Primitives =
    type BlockHeight = BlockHeight of uint32

    and
        #if !NoDUsAsStructs
        [<Struct>]
        #endif
        BlockHeightOffset16 = BlockHeightOffset16 of uint16
