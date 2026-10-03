(*---
fsharp_multi_line_lambda_closing_newline = true
---*)
builder.
    FirstThing<X>(fun lambda ->
        // aaaaaa
        ()
    )
    .SecondThing<Y>(fun next ->
        // bbbbb
        next
    )
    // ccccc
    .ThirdThing<Z>().X
