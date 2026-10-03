let divideByWorkflow x y w z =
    maybe {
        let! a = x |> divideBy y
        let! b = a |> divideBy w
        let! c = b |> divideBy z
        return c
    }
