/// Throws <c>ArgumentException</c>
/// </example>
[<CompiledName("Average")>]
val inline average   : array:^T[] -> ^T   
                            when ^T : (static member ( + ) : ^T * ^T -> ^T) 
                            and  ^T : (static member DivideByInt : ^T*int -> ^T) 
                            and  ^T : (static member Zero : ^T)
