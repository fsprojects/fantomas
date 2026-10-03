namespace Blah

module Foo =
    val inline sum : ('a -> ^value) -> 'a Foo -> ^value
        when ^value : (static member (+) : ^value * ^value -> ^value) and ^value : (static member Zero : ^value)
