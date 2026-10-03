let inline add< ^T, ^U when (^T or ^U): (static member (+): ^T * ^U -> ^T)> (x: ^T) (y: ^U) = x + y
