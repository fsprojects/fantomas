let a1 = [| for i in 1..10 -> i * i |]
let a2 = [| 0..99 |]

let a3 =
    [| for n in 1..100 do
           if isPrime n then yield n |]
