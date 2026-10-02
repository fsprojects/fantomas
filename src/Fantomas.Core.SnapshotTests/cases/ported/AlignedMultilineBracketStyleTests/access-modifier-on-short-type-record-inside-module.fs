(*---
max_line_length = 40
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
module Foo =
    type Stores =
        private {
            ModeratelyLongName : int
        }

    type private Bang = abstract Baz : int
