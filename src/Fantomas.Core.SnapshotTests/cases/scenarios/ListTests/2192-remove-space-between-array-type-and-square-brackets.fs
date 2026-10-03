let x: int [] = [ 1, 2, 3 ]
let x: double []  [] = [ [ 1.0, 2.0, 3.0 ] ]
let foo (x: double []) (y: object [] []) : string [,] = x :> int []
let foo<'T> (x: 'T  []) = x
