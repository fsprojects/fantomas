[<RequireQualifiedAccess; StructuralEquality; StructuralComparison>]
type ILNativeType =
    | UInt64
    | Array of
        ILNativeType option *
        (int32 * int32 option) option (* optional idx of parameter giving size plus optional additive i.e. num elems *)
    | Int
    | UInt
