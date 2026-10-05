(*---
fsharp_max_infix_operator_expression = 60
---*)
let foo =
    let next _ =
        if not animating then activeIndex.update ((activeIndex.current + 1) % itemLength)

    let prev _ =
        if not animating then activeIndex.update ((activeIndex.current + itemLength - 1) % itemLength)

    ()
