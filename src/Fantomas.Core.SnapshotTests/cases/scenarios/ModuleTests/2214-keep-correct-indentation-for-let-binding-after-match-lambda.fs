module Outer

let sort fallback (f: int -> string list) = ()

module Inner =

    let f =
        sort
            "Name"
            (function
             | 1 -> ["One"]
             | _ -> ["Not One"])

    let g () = 23
