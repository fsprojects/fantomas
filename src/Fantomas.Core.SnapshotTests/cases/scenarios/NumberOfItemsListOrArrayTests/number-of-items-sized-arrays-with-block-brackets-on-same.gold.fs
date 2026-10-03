let xs =
    [|
        a
        b
        c
    |]

let ys = [| AReallyLongExpressionThatIsMuchLongerThan50Characters |]

f
    xs
    [|
        x
        y
        z
    |]

List.map
    (fun x -> x * x)
    [|
        1
        2
    |]
