type LongIdentWithDots =
    | //[<Experimental("This construct is subject to change in future versions of FSharp.Compiler.Service and should only be used if no adequate alternative is available.")>]
      LongIdentWithDots of
       leadingId: LongIdent *
       operatorName: OperatorName option *
       trailingId: LongIdent *
       dotRanges: range list
