type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R // without this space the code is invalid
