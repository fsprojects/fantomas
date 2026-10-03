let args =
    match args with
    | [ SynPatErrorSkip(SynPat.Tuple(false, args, _))
         | SynPatErrorSkip(SynPat.Paren(SynPatErrorSkip(SynPat.Tuple(false, args, _)), _)) ] when numArgTys > 1 -> args
    | _ -> failwith "meh"
