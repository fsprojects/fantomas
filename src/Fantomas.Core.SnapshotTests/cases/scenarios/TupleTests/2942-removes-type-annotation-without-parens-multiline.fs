let clReducedValues, // some comment 1
    clFirstActualKeys, // some comment 2
    clSecondActualKeys: ClArray<'a> * ClArray<int> * ClArray<int> =
        reduce processor DeviceOnly resultLength clOffsets clFirstKeys clSecondKeys clValues
