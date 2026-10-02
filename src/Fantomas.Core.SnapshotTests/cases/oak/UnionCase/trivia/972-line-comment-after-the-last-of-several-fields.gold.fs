[<NoEquality; NoComparison; RequireQualifiedAccess>]
type SynType =

    /// F# syntax: A.B.C
    | LongIdent of longDotId: LongIdentWithDots

    /// F# syntax: type<type, ..., type> or type type or (type, ..., type) type
    ///   isPostfix: indicates a postfix type application e.g. "int list" or "(int, string) dict"
    | App of
        typeName: SynType *
        lessRange: range option *
        typeArgs: SynType list *
        commaRanges: range list *
        greaterRange: range option *
        isPostfix: bool *
        range: range // interstitial commas
