(*---
max_line_length = 80
fsharp_multiline_bracket_style = cramped
---*)
let nestedList: obj list = [
    "11111111aaaaaaaaa"
    "22222222aaaaaaaaa"
    "33333333aaaaaaaaa"
    [ // this case looks weird but seen rarely
        "11111111bbbbbbbbbbbbbbb"
        "22222222bbbbbbbbbbbbbbb"
        "33333333bbbbbbbbbbbbbbb"
    ]
]
