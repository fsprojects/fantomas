type A =
    | A of int
    | B of
        {| A: int
           LongerThanLengthDeclaration: string |}
