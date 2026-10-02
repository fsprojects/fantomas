module Heap

type Heap<'T when 'T: comparison> =
    class
        new: capacity: int -> Heap<'T>
        member Clear: unit -> unit
        member ExtractMin: unit -> 'T
        member Insert: k: 'T -> unit
        member IsEmpty: unit -> bool
        member PeekMin: unit -> 'T
        override ToString: unit -> string
        member Count: int
    end
