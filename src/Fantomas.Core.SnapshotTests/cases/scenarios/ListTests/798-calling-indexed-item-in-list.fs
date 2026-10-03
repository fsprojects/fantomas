namespace Foo

type T = { A : (unit -> unit) array }
module F =
  let f (a : T) =
    a.A.[0] ()
