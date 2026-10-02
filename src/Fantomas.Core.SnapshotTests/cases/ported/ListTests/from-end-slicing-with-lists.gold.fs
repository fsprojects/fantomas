let a = list.[..^0] // 1,2,3,4,5
let b = list.[..^1] // 1,2,3,4
let c = list.[0..^1] // 1,2,3,4
let d = list.[^1..] // 4,5
let e = list.[^0..] // 5
let f = list.[^2..^1] // 3,4
