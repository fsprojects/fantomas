(*---
max_line_length = 80
fsharp_multi_line_lambda_closing_newline = true
---*)
let dotted () =
    Cfg.register (fun (aVeryLongParameterName: AnEquallyLongTypeName) (anotherLongParameterName: AnotherTypeName) -> body ())

let undotted () =
    registerWith (fun (aVeryLongParameterName: AnEquallyLongTypeName) (anotherLongParameterName: AnotherTypeName) -> body ())
