let cast<'a> (A: obj[,]) : 'a[,] = A |> Array2D.map unbox
let flatten (A: 'a[,]) = A |> Seq.cast<'a>
let getColumn c (A: _[,]) = flatten A.[*, c..c] |> Seq.toArray
