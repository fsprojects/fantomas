/// Represents additional information for SynExpr.TryWith
[<NoEquality; NoComparison>]
type SynExprTryWithTrivia =
    {
        /// The syntax range of the `try` keyword.
        TryKeyword: range

        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range

        /// The syntax range of the `with` keyword
        WithKeyword: range

        /// The syntax range from the beginning of the `with` keyword till the end of the TryWith expression.
        WithToEndRange: range
    }
