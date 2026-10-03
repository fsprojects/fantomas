let squares = seq { for i in 1..3 -> i * i }

let cubes = seq { for i in 1..3 -> i * i * i }

let squaresAndCubes =
    seq {
        yield! squares
        yield! cubes
    }
