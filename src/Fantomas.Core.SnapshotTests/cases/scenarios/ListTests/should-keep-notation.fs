(*---
fsharp_multiline_bracket_style = cramped
---*)
let environVars target =
    [for e in Environment.GetEnvironmentVariables target ->
        let e1 = e :?> Collections.DictionaryEntry
        e1.Key, e1.Value]
    