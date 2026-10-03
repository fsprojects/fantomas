(*---
fsharp_multiline_bracket_style = stroustrup
---*)
let someTest input1 input2 =
    test "This can contain a quite long description of what the test exactly does and why it exists" {
        Expect.equal input1 input2 "didn't equal"
    }
