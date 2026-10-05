type ObjectGraphFormatter(opts: FormatOptions, bindingFlags) =
    let rec nestedObjL depthLim prec (x: obj, ty: Type) = objL ShowAll depthLim prec (x, ty)

    and stringValueL (s: string) =
        countNodes 1
        #if COMPILER
        ()
    #else
    #endif

    and arrayValueL depthLim (arr: Array) = ()
