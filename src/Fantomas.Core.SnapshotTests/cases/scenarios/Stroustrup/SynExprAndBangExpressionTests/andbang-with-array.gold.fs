collect {
    let! abc = def ()

    and! items = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]

    return items
}
