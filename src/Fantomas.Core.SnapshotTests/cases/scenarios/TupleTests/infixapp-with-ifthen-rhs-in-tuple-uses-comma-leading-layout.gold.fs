let a = 0
let b = true
let c = 1
let d = 2

let _ =
    try
        a <> if b then c else d
        , b
    with ex ->
        false, false
