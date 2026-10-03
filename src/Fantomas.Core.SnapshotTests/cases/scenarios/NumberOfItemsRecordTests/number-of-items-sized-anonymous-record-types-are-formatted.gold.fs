let f
    (x:
        {| x: int
           y: obj |})
    =
    x

let g (x: {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}) = x

type A =
    {| x: int
       y: obj |}

type B = {| x: AReallyLongTypeThatIsMuchLongerThan40Characters |}
