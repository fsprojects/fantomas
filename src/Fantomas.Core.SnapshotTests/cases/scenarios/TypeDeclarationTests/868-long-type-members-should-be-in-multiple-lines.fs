(*---
max_line_length = 80
fsharp_space_before_colon = true
---*)
type C() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: int, aSecondVeryLongType: int, aThirdVeryLongType: int) : int =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
