let a = Array.create 10 -1
let b = Array.create 10 a

printfn "%d -> %d" a[0] (b[0][0])
