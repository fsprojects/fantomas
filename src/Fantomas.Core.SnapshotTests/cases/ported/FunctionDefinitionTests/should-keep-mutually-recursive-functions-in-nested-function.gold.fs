let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
