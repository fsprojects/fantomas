type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) : AVeryLongReturnType =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
