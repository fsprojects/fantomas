module internal FSharp.Compiler.TypedTreePickle

/// Deserialize a tuple
val inline internal u_tup4:
    unpickler<'T2> -> unpickler<'T3> -> unpickler<'T4> -> unpickler<'T5> -> unpickler<'T2 * 'T3 * 'T4 * 'T5>
