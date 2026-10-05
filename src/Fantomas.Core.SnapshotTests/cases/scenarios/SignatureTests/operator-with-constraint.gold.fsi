namespace Bar

val inline (.+.) : x : ^a Foo -> y : ^b Foo -> ^c Foo when (^a or ^b) : (static member (+) : ^a * ^b -> ^c)
