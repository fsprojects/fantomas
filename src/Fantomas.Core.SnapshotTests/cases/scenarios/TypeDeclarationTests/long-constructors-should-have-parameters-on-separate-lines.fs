(*---
fsharp_space_before_class_constructor = true
---*)
type C (aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) =
    member this.X = 42
