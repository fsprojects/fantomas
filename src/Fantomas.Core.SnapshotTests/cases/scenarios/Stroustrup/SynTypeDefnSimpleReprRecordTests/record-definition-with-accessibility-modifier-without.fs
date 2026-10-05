(*---
fsharp_multiline_bracket_style = stroustrup
---*)
type NonEmptyList<'T> =
    private
        { List: 'T list; Value: 'T; Third: string}
