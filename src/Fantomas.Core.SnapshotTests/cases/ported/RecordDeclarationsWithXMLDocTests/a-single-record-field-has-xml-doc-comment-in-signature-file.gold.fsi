type SynExprTryWithTrivia =
    {
        TryKeyword: range
        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
