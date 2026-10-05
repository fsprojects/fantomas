let user_printers =
    ref ([]: (string * (term -> unit)) list)

let the_interface =
    ref ([]: (string * (string * hol_type)) list)
