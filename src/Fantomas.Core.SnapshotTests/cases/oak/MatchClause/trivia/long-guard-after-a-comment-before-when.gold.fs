match x with
| a
    // c
    when
        someVeryLongConditionNameNumberOne
        && someVeryLongConditionNameNumberTwo
        && someVeryLongConditionNameNumberThree
        ->
        1
| _ -> 0
