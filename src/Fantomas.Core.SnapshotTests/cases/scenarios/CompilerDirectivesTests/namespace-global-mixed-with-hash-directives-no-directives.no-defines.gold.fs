namespace global

#if DEBUG






#else
module Dbg =
    let tee (f: 'a -> unit) (x: 'a) = x
    let teePrint x = x
    let print _ = ()
#endif
