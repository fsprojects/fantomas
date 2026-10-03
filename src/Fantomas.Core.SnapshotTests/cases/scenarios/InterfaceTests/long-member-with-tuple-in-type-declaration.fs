(*---
max_line_length = 60
fsharp_space_before_colon = true
---*)
type IFoo =
    abstract Bar : [<Path "bar">] bar : string  * [<Path "baz">] baz : string ->  Task<Foo>
