let a1 = [| 0..99 |]

let a2 =
    [|
        for n in 1..100 do
            if isPrime n then yield n
    |]
