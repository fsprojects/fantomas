type T = A | B

let f a  =
    match a with
    | (A | B as bi, x) ->
        1
