// This function recognises these "infix operator" names.
let IsMangledInfixOperator mangled = (* where mangled is assumed to be a compiled name *)
    let afterSkipStarts prefixes =
        Array.exists afterSkipStartsWith prefixes

    s <> mangled
    && (s = ":="
        || // COLON_EQUALS
        afterSkipStarts relational
        || // EQUALS, INFIX_COMPARE_OP, LESS, GREATER
        s = "$"
        || // DOLLAR
        afterSkipStarts concat
        || // INFIX_AT_HAT_OP
        s = "**") // INFIX_STAR_STAR_OP
