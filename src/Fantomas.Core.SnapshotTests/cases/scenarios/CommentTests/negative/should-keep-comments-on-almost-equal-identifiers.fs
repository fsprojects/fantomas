let zp = p1 ``lxor`` p2
// Comment 1
let b = zp ``land`` (zp)
(* Comment 2 *)
let p = p1 ``land`` (b - 1)
