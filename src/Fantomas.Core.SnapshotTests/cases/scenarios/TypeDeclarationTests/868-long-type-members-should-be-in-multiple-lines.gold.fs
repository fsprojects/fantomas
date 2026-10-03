type C() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType : int,
            aSecondVeryLongType : int,
            aThirdVeryLongType : int
        ) : int =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
