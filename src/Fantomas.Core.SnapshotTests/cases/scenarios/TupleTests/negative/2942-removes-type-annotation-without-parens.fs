let clReducedValues, clFirstActualKeys, clSecondActualKeys: ClArray<'a> * ClArray<int> * ClArray<int> =
    reduce processor DeviceOnly resultLength clOffsets clFirstKeys clSecondKeys clValues
