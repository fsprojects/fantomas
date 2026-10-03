(*---
fsharp_space_before_class_constructor = true
---*)
type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =  aVeryLongType aSecondVeryLongType aThirdVeryLongType
