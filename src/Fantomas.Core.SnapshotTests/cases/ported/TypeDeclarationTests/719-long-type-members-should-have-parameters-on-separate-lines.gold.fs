type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) =
        aVeryLongType aSecondVeryLongType aThirdVeryLongType
