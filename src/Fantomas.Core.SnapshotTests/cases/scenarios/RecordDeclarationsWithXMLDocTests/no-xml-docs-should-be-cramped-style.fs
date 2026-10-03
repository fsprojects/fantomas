(*---
fsharp_multiline_bracket_style = cramped
---*)
type SynExprTryWithTrivia =
    {
        TryKeyword: range
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
