(*---
fsharp_space_before_class_constructor = true
fsharp_space_before_colon = true
fsharp_alternative_long_member_definitions = true
---*)
type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
