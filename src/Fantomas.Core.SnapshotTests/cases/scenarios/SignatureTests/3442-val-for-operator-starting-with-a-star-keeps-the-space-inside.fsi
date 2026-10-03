module MahSignature

val inline (.*)  : (unit -> ^a) -> ^b           -> unit -> ^c
val inline ( *.) : ^a           -> (unit -> ^b) -> unit -> ^c
val inline (.*.) : (unit -> ^a) -> (unit -> ^b) -> unit -> ^c
