module Example

module Foo =
    module Bar =
        type t = bool
        val lol: unit -> bool

    type t = int
    val lmao: unit -> bool

module Foo2 =
    module Bar =
        type t = bool

        val lol: unit -> bool

    type t = int

    val lmao: unit -> bool
