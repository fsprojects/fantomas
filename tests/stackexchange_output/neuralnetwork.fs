/// The program is from http://codereview.stackexchange.com/q/12643
module nnbasic

let mutable neuroninput = 
  [0.0; 0.0]

let mutable weight = 
  [0.4; 0.6]

let rate = 0.2

let threeshold = 2.0

let matrix = 
  [[0.0; 0.0; 0.0]
   [0.0; 1.0; 1.0]
   [1.0; 0.0; 1.0]
   [1.0; 1.0; 1.0]]

let display output real = if output = real then printfn "yes" else printfn "no"

let output(_ni : float list, _wi : float list) = if threeshold > _ni.[0] * _wi.[0] + _ni.[1] * _wi.[1] then 0.0 else 1.0

let mutable iter = 0

let mutable out = 0.0

while iter < 100 do
  for row in matrix do
    neuroninput <- [row.[0]
                    row.[1]]
    out <- output(neuroninput, weight)
    weight <- [weight.[0] + rate * (row.[2] - out)
               weight.[1]]
    display out row.[2]
    out <- output(neuroninput, weight)
    weight <- [weight.[0]
               weight.[1] + rate * (row.[2] - out)]
    if threeshold > neuroninput.[0] * weight.[0] + neuroninput.[1] * weight.[1] then display 0.0 row.[2] else display 1.0 row.[2]
    iter <- iter + 1