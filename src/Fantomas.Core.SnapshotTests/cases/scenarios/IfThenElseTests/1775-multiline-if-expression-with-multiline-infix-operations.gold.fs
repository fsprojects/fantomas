if
    hasClassAttr
    && not (
        match k with
        | SynTypeDefnKind.Class -> true
        | _ -> false
    )
    || hasMeasureAttr
       && not (
           match k with
           | SynTypeDefnKind.Class
           | SynTypeDefnKind.Abbrev
           | SynTypeDefnKind.Opaque -> true
           | _ -> false
       )
    || hasInterfaceAttr
       && not (
           match k with
           | SynTypeDefnKind.Interface -> true
           | _ -> false
       )
    || hasStructAttr
       && not (
           match k with
           | SynTypeDefnKind.Struct
           | SynTypeDefnKind.Record
           | SynTypeDefnKind.Union -> true
           | _ -> false
       )
then
    error (Error(FSComp.SR.tcKindOfTypeSpecifiedDoesNotMatchDefinition (), m))

k
