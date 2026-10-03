type IlxUnionHasHelpers =
    | NoHelpers
    | AllHelpers
    | SpecialFSharpListHelpers
    | SpecialFSharpOptionHelpers

/// Union references
type IlxUnionRef =
    | IlxUnionRef of
        boxity: ILBoxity *
        ILTypeRef *
        IlxUnionCase[] *
        bool (* IsNullPermitted *) *
        IlxUnionHasHelpers (* HasHelpers *)

type IlxUnionSpec =
    | IlxUnionSpec of IlxUnionRef * ILGenericArgs

    member DeclaringType: ILType

    member GenericArgs: ILGenericArgs

    member Alternatives: IlxUnionCase list
