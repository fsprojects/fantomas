let a = [ 2 .. 7 ] // integers
let b = [ one .. two ] // identifiers
let c = [ .. 9 ] // also when there is only one expression
let d = [ 0.7 .. 9.2 ] // doubles
let e = [ 2L .. number / 2L ] // complex expression
let f = [| A.B .. C.D |] // identifiers with dots
let g = [ .. (39 - 3) ] // complex expression
let h = [| 1 .. MyModule.SomeConst |] // not all expressions are atomic
for x in 1 .. 2 do
    printfn " x = %d" x
let s = seq { 0..10..100 }
