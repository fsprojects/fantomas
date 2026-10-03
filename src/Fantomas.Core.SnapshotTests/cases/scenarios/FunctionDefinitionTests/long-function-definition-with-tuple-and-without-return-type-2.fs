(*---
fsharp_space_before_colon = true
fsharp_align_function_signature_to_indentation = true
---*)
let longFunctionWithLongTupleParameter
    (aVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aSecondVeryLongParam: AVeryLongTypeThatYouNeedToUse,
     aThirdVeryLongParam: AVeryLongTypeThatYouNeedToUse)
    =
    // ... the body of the method follows
    ()
