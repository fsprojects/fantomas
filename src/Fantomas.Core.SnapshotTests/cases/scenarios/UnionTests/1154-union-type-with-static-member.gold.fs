type CardValue =
    | Basic of int
    | Jack
    | Knight
    | Queen
    | King
    static member allWithKnight =
        [
            for n in 1..10 do
                yield Basic n
            yield Jack
            yield Knight
            yield Queen
            yield King
        ]
