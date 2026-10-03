let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) veryVeryVeryVeryVeryVeryVeryVeryVeryLongThing)
        >> printfn "%d"
    )

let printListWithOffset' a list1 =
    list1
    |> List.iter (((+) a) >> printfn "%d")

let foldList a list1 =
    list1
    |> List.fold
        (((+) a) >> printfn "%d")
        someVeryLongAccumulatorNameThatMakesTheWholeConstructMultilineBecauseOfTheLongName
