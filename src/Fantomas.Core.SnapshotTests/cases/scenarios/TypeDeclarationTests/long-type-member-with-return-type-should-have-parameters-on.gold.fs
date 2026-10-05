type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) : int =
        aVeryLongType aSecondVeryLongType aThirdVeryLongType
