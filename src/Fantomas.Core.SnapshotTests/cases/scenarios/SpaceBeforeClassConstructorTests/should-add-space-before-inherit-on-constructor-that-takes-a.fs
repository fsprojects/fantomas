(*---
fsharp_multiline_bracket_style = cramped
---*)
type DerivedClass =
    inherit BaseClass

    val string2: string

    new (str1, str2) =
        { inherit BaseClass "meh"
          string2 = str2 }
