namespace foo

type SynTypeConstraint =

    /// F# syntax: is 'typar: struct
    | WhereTyparIsValueType of typar: SynTypar * range: range
