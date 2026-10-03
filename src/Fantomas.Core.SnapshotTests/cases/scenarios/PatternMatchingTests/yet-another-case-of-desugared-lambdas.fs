let UNIFY_ACCEPT_TAC mvs th (asl, w) =
    let insts = term_unify mvs (concl th) w
    ([], insts), [],
    let th' = INSTANTIATE insts th
    fun i [] -> INSTANTIATE i th'