let inline add (value1: ^T when ^T: (static member (+): ^T * ^T -> ^T), value2: ^T) = value1 + value2

let inline heterogenousAdd (value1: ^T when (^T or ^U): (static member (+): ^T * ^U -> ^T), value2: ^U) =
    value1 + value2
