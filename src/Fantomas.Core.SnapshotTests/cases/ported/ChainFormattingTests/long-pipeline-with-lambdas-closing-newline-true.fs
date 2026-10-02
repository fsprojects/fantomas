(*---
max_line_length = 40
fsharp_multi_line_lambda_closing_newline = true
---*)
builder.FirstThing<X>(fun lambda -> processFirst lambda).SecondThing<Y>(fun next -> processSecond next).ThirdThing<Z>().Result
