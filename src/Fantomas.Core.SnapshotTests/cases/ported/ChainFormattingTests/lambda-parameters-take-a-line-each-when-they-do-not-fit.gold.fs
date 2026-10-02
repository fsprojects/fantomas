let dotted () =
    Cfg.register
        (fun
            (aVeryLongParameterName: AnEquallyLongTypeName)
            (anotherLongParameterName: AnotherTypeName) -> body ())

let undotted () =
    registerWith
        (fun
            (aVeryLongParameterName: AnEquallyLongTypeName)
            (anotherLongParameterName: AnotherTypeName) -> body ())
