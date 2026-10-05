(*---
max_line_length = 40
fsharp_space_before_uppercase_invocation = true
fsharp_multiline_bracket_style = cramped
---*)
module Foo =
    type Stores =
        private {
            ModeratelyLongName : int
        }

    type private Bang = abstract Baz : int
