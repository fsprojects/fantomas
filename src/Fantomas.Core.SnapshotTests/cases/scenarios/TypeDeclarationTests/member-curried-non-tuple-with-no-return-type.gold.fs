type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (aVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse)
        =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
