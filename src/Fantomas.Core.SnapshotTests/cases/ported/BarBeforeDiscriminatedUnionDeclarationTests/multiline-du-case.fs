(*---
fsharp_bar_before_discriminated_union_declaration = true
---*)
[<NoEquality; NoComparison>]
type SynBinding =
    SynBinding of
                        accessibility: SynAccess option *
                        kind: SynBindingKind *
                        mustInline: bool *
                        isMutable: bool *
                        attributes: SynAttributes *
                        xmlDoc: PreXmlDoc *
                        valData: SynValData *
                        headPat: SynPat *
                        returnInfo: SynBindingReturnInfo option *
                        expr: SynExpr *
                        range: range *
                        seqPoint: DebugPointAtBinding
