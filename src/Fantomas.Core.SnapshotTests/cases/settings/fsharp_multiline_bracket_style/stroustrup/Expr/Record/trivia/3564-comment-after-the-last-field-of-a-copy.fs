(*---
# The closing brace goes back to the column of the binding, as it does without the comment.
fsharp_multiline_bracket_style = stroustrup
---*)
let updated =
    { someRecordValue with
        FieldNumberOne = 1
        FieldNumberTwo = 2
        // trailing comment
    }
