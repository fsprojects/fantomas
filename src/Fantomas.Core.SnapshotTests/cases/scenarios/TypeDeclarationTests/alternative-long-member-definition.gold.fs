type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse
        )
        =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
